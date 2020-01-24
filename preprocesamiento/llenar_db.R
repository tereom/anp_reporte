library(DBI)
library(tidyverse)
library(sf)
library(fs)

# ---------------- conexión a base de datos ------------
dotenv::load_dot_env(".env")
PGDATABASE = Sys.getenv("PGDATABASE")
POSTGRES_PASSWORD = Sys.getenv('POSTGRES_PASSWORD')
POSTGRES_USER = Sys.getenv('POSTGRES_USER')
PGHOST = Sys.getenv('PGHOST')

# RPostgres::Postgres()
con <- dbConnect(RPostgres::Postgres(),
    host = PGHOST,
    dbname = PGDATABASE,
    user = POSTGRES_USER,
    password = POSTGRES_PASSWORD
)
dbListTables(con)

# ---------------- crear tablas db para unir datos ------------
lista_anps <- tbl(con, "lista_anps")
lista_anps_ids <- lista_anps %>% 
    select(id, id_07, nombre) %>% 
    collect()

anp_tipo <- tbl(con, "anp_tipo")
glimpse(anp_tipo)
tipo <- tbl(con, "tipo") %>% 
    collect()

anps_ids <- anp_tipo %>%
    left_join(lista_anps_ids, by = c("lista_anps_id" = "id"), copy = TRUE) %>% 
    collect() %>% 
    rename(anp_tipo_id = id)
write_csv(anps_ids, "datos_procesados/anps_ids.csv")
View(anps_ids)

# ---------------- crear tablas shapes para unir datos ------------
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

names(anps_shp) <- anps_shp
nombres_anp <- map_df(anps_shp, function(x){
    sf_anp <- st_read(str_c(path_anps_shp, "/", x, ".shp"), 
        stringsAsFactors = FALSE) %>% 
        as_tibble() %>% 
        dplyr::select(id_07 = ID_07, nombre = NOMBRE)}, .id = "anp") %>% 
    distinct()
nombres <- readr::read_delim("datos_insumo/anp_nombres.tsv", "\t", 
    escape_double = FALSE, trim_ws = TRUE)
nombres_anp <- nombres %>%
    left_join(nombres_anp, by = c("anp_sin_acentos" = "anp")) %>% 
    select(id_07, anp = anp_sin_acentos, nombre)
nombres_anp[nombres_anp$anp == "anp_terrestres_2017_NOMBRE_Islas_del_Pacifico_de_la_Peninsula_de_Baja_California", ]$id_07 <- 54321
nombres_anp[nombres_anp$anp == "anp_terrestres_2017_NOMBRE_Sierra_de_Tamaulipas", ]$id_07 <- 12345

write_csv(nombres_anp, path = "datos_procesados/nombres_anps_id.csv")

# ---------------- escribir tabla ecorregión ------------
load(file = "datos_procesados/2018-08-08_ecorregion.RData")
ecoregion_anp_df <- anp_eco_df %>% 
    ungroup() %>% 
    mutate(tipo_id = 1, nombre = NOMBRE, hectareas = round(hectareas, 3)) %>% 
    left_join(anps_ids, by = c("tipo_id", "id_07", "nombre")) %>%
    select(anp_tipo_id, ecoregion = eco, hectareas) %>% 
    na.omit()

dbWriteTable(conn = con, name = "ecoregion_anp", value = ecoregion_anp_df, 
    row.names = FALSE, append = TRUE)

# ---------------- escribir tabla perdida cobertura ------------
load("datos_procesados/2018-09-05_perdida_cobertura.RData")
load("datos_procesados/2018-09-05_perdida_cobertura_rings.RData")
load("datos_procesados/2018-09-05_perdida_cobertura_zn.RData")
load("datos_procesados/2018-09-05_perdida_cobertura_zp.RData")

cobertura_df <- function(perdida_df, tipo){
    perdida_df <- perdida_df %>% 
        mutate(tipo_id = tipo) %>%
        left_join(nombres_anp, by = "anp") %>%
        left_join(anps_ids, by = c("tipo_id", "id_07", "nombre")) %>%
        select(anp_tipo_id, anp, nombre, id_07, tipo_id,
            clase_madmex, anio_perdida = year_loss, clase_madmex, n) %>%
        na.omit()
}

cobertura_list <- map2(list(perdida_anps_df, perdida_anps_rings_df, 
    perdida_anps_zn_df, perdida_anps_zp_df), 1:4, cobertura_df)

cobertura <- bind_rows(cobertura_list) 
write_csv(cobertura, path = "datos_procesados/2018-09-06_cobertura.csv")

cobertura_db <- cobertura %>% 
    select(anp_tipo_id, anio_perdida, clase_madmex, n)
dbWriteTable(conn = con, name = "perdida_anp", 
    value = cobertura_db, row.names=FALSE, append = TRUE)


# ---------------- escribir tabla integridad ------------
# primero creamos csv de estadísticas que no se ingesta
load("datos_procesados/2018-09-04.RData")
extrae_df <- function(ie_list_type, tipo){
    ie_stats <- map(ie_list_type, function(x){ x$valores <- NULL; x})
    ie_df <- bind_rows(!!!ie_stats) %>%
        mutate(
            tipo_id = tipo, 
            anp = str_replace(anp, pattern = "_ring", "")
        ) %>%
        left_join(nombres_anp, by = "anp") %>%
        left_join(anps_ids, by = c("tipo_id", "id_07", "nombre")) %>%
        select(anp_tipo_id, anp, nombre, id_07, tipo_id, anio,
            media, mediana, desv_est = desv.est) %>%
        na.omit()
}

ie_stats_list <- map2(list(ie_list, ie_anillos_list, ie_zn_list, ie_zp_list), 
    1:4, extrae_df)

map(list(ie_list, ie_anillos_list, ie_zn_list, ie_zp_list), length)
map(ie_stats_list, nrow)
ie_df <- bind_rows(!!!ie_stats_list) 
write_csv(ie_df, 
    path = "datos_procesados/2018-07-27_ie_stats.csv")
dbWriteTable(conn = con, name = "ie_list_valores_anp", 
    value = ie_list_valores, row.names = FALSE, append = TRUE)

# integridad ie_list_valores si se ingesta    
extrae_valores <- function(ie_list_type, tipo){
    ie_valores <- map_df(ie_list_type, function(x){ data_frame(anp = x$anp, 
        valores = x$valores) })
    ie_df <- ie_valores %>%
        mutate(
            tipo_id = tipo, 
            anp = str_replace(anp, pattern = "_ring", "")
        ) %>%
        left_join(nombres_anp, by = "anp") %>%
        left_join(anps_ids, by = c("tipo_id", "id_07", "nombre")) %>%
        select(anp_tipo_id, anp, nombre, id_07, tipo_id, valores) %>%
        na.omit()
}

ie_list_all_values <- map2(list(ie_list, ie_anillos_list, ie_zn_list,
    ie_zp_list), 1:4, extrae_valores)

ie_list_valores_csv <- bind_rows(ie_list_all_values)
write_csv(ie_list_valores_csv, 
        path = "datos_procesados/2018-07-27_ie_list_valores_anp.csv")

ie_list_valores <- select(ie_list_valores_csv, anp_tipo_id, valores)
dbWriteTable(conn = con, name = "ie_list_valores_anp", 
    value = ie_list_valores, row.names = FALSE, append = TRUE)

### Idoneidad de hábitat
load("datos_procesados/2018-02-12_suitability.RData")

crear_tablas_suit <- function(tabla_suit, tipo){
    tabla_suit_ids <- tabla_suit %>% 
        add_column(tipo_id = tipo) %>%
        left_join(nombres_anp, by = "anp") %>%
        left_join(anps_ids, by = c("tipo_id", "id_07", "nombre")) %>%
        dplyr::select(anp_tipo_id, anio, idoneidad) %>%
        na.omit()
} 
ih_list_all <- map2(list(tabla_suit, tabla_suit_rings, 
    tabla_suit_zn, tabla_suit_zp), 1:4, 
    crear_tablas_suit)
ih_df <- bind_rows(!!!ih_list_all) 

write_csv(ih_df, 
    path = "datos_procesados/2018-07-27_idoneidad_anp.csv")

dbWriteTable(conn = con, name = "idoneidad_anp", value = ih_df, 
    row.names = FALSE, append = TRUE)

# Idoneidad de hábitat ponca
# aún no hay estructura para ponca en base de datos
load("datos_procesados/2019-01-25_suitability_ponca.RData")
ih_list_all <- map2(list(tabla_suit, tabla_suit_rings, 
    tabla_suit_zn, tabla_suit_zp), 1:4, 
    crear_tablas_suit)
ih_df <- bind_rows(!!!ih_list_all) 
write_csv(ih_df, path = "datos_procesados/2019-02-14_idoneidad_anp_ponca.csv")

#### agregado para descarga no para db
load("datos_procesados/2018-08-24_suitabilility_report.RData")

stats_puma <- suit_stats %>% 
    left_join(nombres_anp, by = "anp") %>%
    left_join(anps_ids, by = c("id_07", "nombre")) %>%
    dplyr::select(id_07, anp, nombre, poligono = tipo, media:desv_est)
anps_puma <- read_csv("datos_insumo/calidad_habitat/puma_ponca_anps.csv") %>% 
    filter(!is.na(`Puma concolor`))
stats_puma %>% 
    semi_join(anps_puma, by = c("id_07" = "ID_07")) %>% 
    write_csv(path = "datos_procesados/descarga/calidad_habitat_puma.csv")
rm(suit_stats, tabla_suit)

load("datos_procesados/2019-02-14_suitabilility_report_ponca.RData")

stats_ponca <- suit_stats %>% 
    left_join(nombres_anp, by = "anp") %>%
    left_join(anps_ids, by = c("id_07", "nombre")) %>%
    dplyr::select(id_07, anp, nombre, poligono = tipo, media:desv_est)
anps_ponca <- read_csv("datos_insumo/calidad_habitat/puma_ponca_anps.csv") %>% 
    filter(!is.na(`Panthera onca`))
stats_ponca %>% 
    semi_join(anps_ponca, by = c("id_07" = "ID_07")) %>% 
    write_csv(path = "datos_procesados/descarga/calidad_habitat_ponca.csv")
