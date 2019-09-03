library(raster)
library(dplyr)
library(stringr)
library(purrr)
library(rslurm)
library(lubridate)
library(readr)
library(ggplot2)
library(stringr)
library(readr)
library(sf)
library(tidyr)

# crear brick con mapas
suit_files <- list.files("datos_insumo/calidad_habitat/puma", 
    pattern = "suitability.*tif$", full.names = TRUE)
suit_files <- set_names(suit_files, str_c("anio_", parse_number(suit_files)))[5:11]
suit_rasters <- map(suit_files, raster::raster)
suit_brick <- raster::stack(suit_rasters)

anp_suitability <- function(anp_shp){
    anp  <- st_read(anp_shp, stringsAsFactors = FALSE) %>% distinct() 
    anp_suit <- suit_brick %>% 
        raster::crop(anp) %>% 
        raster::mask(mask = anp)
    anp_suit_df <- data.frame(
        anp = basename(tools::file_path_sans_ext(anp_shp)), 
        raster::rasterToPoints(anp_suit), 
        stringsAsFactors = FALSE)
    anp_suit_df_sample <- sample_n(anp_suit_df, 
        size = min(1000, nrow(anp_suit_df))) %>% 
        gather(anio, idoneidad, anio_2008:anio_2014) %>% 
        mutate(anio = parse_number(anio), 
            idoneidad = round(idoneidad, 3))
    return(anp_suit_df_sample)
}

quitar <- c("anp_terrestres_2017_NOMBRE_El_Sabinal", 
    "anp_terrestres_2017_NOMBRE_Fuentes_Brotantes_de_Tlalpan", 
    "anp_terrestres_2017_NOMBRE_Isla_Isabel", 
    "anp_terrestres_2017_NOMBRE_Lago_de_Camecuaro",
    "anp_terrestres_2017_NOMBRE_Los_Novillos",
    "anp_terrestres_2017_NOMBRE_Molino_de_Flores_Netzahualcoyotl",
    "anp_terrestres_2017_NOMBRE_Playa_Ceuta", 
    "anp_terrestres_2017_NOMBRE_Playa_Cuitzmala",
    "anp_terrestres_2017_NOMBRE_Playa_de_la_Isla_Contoy",
    "anp_terrestres_2017_NOMBRE_Playa_El_Tecuan",
    "anp_terrestres_2017_NOMBRE_Playa_El_Verde_Camacho",
    "anp_terrestres_2017_NOMBRE_Playa_Mexiquillo",
    "anp_terrestres_2017_NOMBRE_Playa_Teopa",
    "anp_terrestres_2017_NOMBRE_Rayon",
    "anp_terrestres_2017_NOMBRE_Tula", 
    "anp_terrestres_2017_NOMBRE_Islas_del_Golfo_de_California"
    )
# anps
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
anps_correr <- anps_shp[!(basename(tools::file_path_sans_ext(anps_shp)) %in% quitar)]
tabla_suit <- map_df(anps_correr, anp_suitability)

# anillos
path_anps_shp <- "datos_insumo/shapes_anp/anp_rings"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
anps_correr <- anps_shp[!(str_replace(basename(tools::file_path_sans_ext(anps_shp)), "_ring", "") %in% quitar)]
tabla_suit_rings <- map_df(anps_correr, anp_suitability)
tabla_suit_rings <- tabla_suit_rings %>% 
    mutate(anp = str_replace(anp, pattern = "_ring", ""))

# zonas núcleo
path_anps_shp <- "datos_insumo/shapes_anp/anp_zonasnucleo"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
anps_correr <- anps_shp[!(basename(tools::file_path_sans_ext(anps_shp)) %in% quitar)]
tabla_suit_zn <- map_df(anps_correr, anp_suitability)

# zonas preservación
path_anps_shp <- "datos_insumo/shapes_anp/anp_zonaspreservacion"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
anps_correr <- anps_shp[!(basename(tools::file_path_sans_ext(anps_shp)) %in% quitar)]
tabla_suit_zp <- map_df(anps_correr, anp_suitability)

# info para base de datos
save(tabla_suit, tabla_suit_rings, tabla_suit_zn, tabla_suit_zp,
    file = "datos_procesados/2018-02-12_suitability.RData")

# tablas para reportes
load("datos_procesados/2018-02-12_suitability.RData")

crear_tablas_suit <- function(tabla_suit, tipo){
    tabla_suit_ids <- tabla_suit %>% 
        mutate(tipo_id = tipo) %>%
        na.omit()
    } 

suit_df_all <- purrr::map2_df(list(tabla_suit, tabla_suit_rings, tabla_suit_zn, 
    tabla_suit_zp), c("ANP", "anillo", "núcleo", "preservación"), 
    crear_tablas_suit)

suit_stats <- suit_df_all %>% 
    group_by(anp, tipo_id, anio) %>% 
    summarise(
        media = mean(idoneidad), 
        mediana = median(idoneidad), 
        desv_est = sd(idoneidad)
        ) %>% 
    rename(tipo = tipo_id)

save(tabla_suit, suit_stats, 
    file = "datos_procesados/2018-08-24_suitabilility_report.RData")


### ponca

# crear brick con mapas
suit_files <- list.files("datos_insumo/temporal_modeling_ponca_B1", 
    pattern = "suitability.tif$", full.names = TRUE, recursive = TRUE)
suit_files <- set_names(suit_files, str_c("anio_", 
    parse_number(str_extract(suit_files, "/[0-9]+"))))
suit_rasters <- map(suit_files, raster::raster)
suit_brick <- raster::stack(suit_rasters)

suit_brick_longlat <- projectRaster(suit_brick, 
    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

anp_suitability <- function(anp_shp){
    anp  <- st_read(anp_shp, stringsAsFactors = FALSE) %>% distinct() 
    anp_suit <- suit_brick_longlat %>% 
        raster::crop(anp) %>% 
        raster::mask(mask = anp)
    anp_suit_df <- data.frame(
        anp = basename(tools::file_path_sans_ext(anp_shp)), 
        raster::rasterToPoints(anp_suit), 
        stringsAsFactors = FALSE)
    anp_suit_df_sample <- sample_n(anp_suit_df, 
        size = min(1000, nrow(anp_suit_df))) %>% 
        gather(anio, idoneidad, anio_2008:anio_2014) %>% 
        mutate(anio = parse_number(anio), 
            idoneidad = round(idoneidad, 3))
    return(anp_suit_df_sample)
}

quitar <- c("anp_terrestres_2017_NOMBRE_El_Sabinal", 
    "anp_terrestres_2017_NOMBRE_Fuentes_Brotantes_de_Tlalpan", 
    "anp_terrestres_2017_NOMBRE_Isla_Isabel", 
    "anp_terrestres_2017_NOMBRE_Lago_de_Camecuaro",
    "anp_terrestres_2017_NOMBRE_Los_Novillos",
    "anp_terrestres_2017_NOMBRE_Molino_de_Flores_Netzahualcoyotl",
    "anp_terrestres_2017_NOMBRE_Playa_Ceuta", 
    "anp_terrestres_2017_NOMBRE_Playa_Cuitzmala",
    "anp_terrestres_2017_NOMBRE_Playa_de_la_Isla_Contoy",
    "anp_terrestres_2017_NOMBRE_Playa_El_Tecuan",
    "anp_terrestres_2017_NOMBRE_Playa_El_Verde_Camacho",
    "anp_terrestres_2017_NOMBRE_Playa_Mexiquillo",
    "anp_terrestres_2017_NOMBRE_Playa_Teopa",
    "anp_terrestres_2017_NOMBRE_Rayon",
    "anp_terrestres_2017_NOMBRE_Tula", 
    "anp_terrestres_2017_NOMBRE_Islas_del_Golfo_de_California"
)
# anps
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
anps_correr <- anps_shp[!(basename(tools::file_path_sans_ext(anps_shp)) %in% quitar)]
tabla_suit <- map_df(anps_correr, anp_suitability)

# anillos
path_anps_shp <- "datos_insumo/shapes_anp/anp_rings"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
anps_correr <- anps_shp[!(str_replace(basename(tools::file_path_sans_ext(anps_shp)), "_ring", "") %in% quitar)]
tabla_suit_rings <- map_df(anps_correr, anp_suitability)
tabla_suit_rings <- tabla_suit_rings %>% 
    mutate(anp = str_replace(anp, pattern = "_ring", ""))

# zonas núcleo
path_anps_shp <- "datos_insumo/shapes_anp/anp_zonasnucleo"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
anps_correr <- anps_shp[!(basename(tools::file_path_sans_ext(anps_shp)) %in% quitar)]
tabla_suit_zn <- map_df(anps_correr, anp_suitability)

# zonas preservación
path_anps_shp <- "datos_insumo/shapes_anp/anp_zonaspreservacion"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
anps_correr <- anps_shp[!(basename(tools::file_path_sans_ext(anps_shp)) %in% 
        c(quitar, "anp_terrestres_2017_NOMBRE_Constitucion_de_1857"))]
tabla_suit_zp <- map_df(anps_correr, anp_suitability)

# info para base de datos
save(tabla_suit, tabla_suit_rings, tabla_suit_zn, tabla_suit_zp,
    file = "datos_procesados/2019-01-25_suitability_ponca.RData")

# tablas para reportes
load("datos_procesados/2019-01-25_suitability_ponca.RData")

suit_df_all <- purrr::map2_df(list(tabla_suit, tabla_suit_rings, tabla_suit_zn, 
    tabla_suit_zp), c("ANP", "anillo", "núcleo", "preservación"), 
    crear_tablas_suit)

suit_stats <- suit_df_all %>% 
    group_by(anp, tipo_id, anio) %>% 
    summarise(
        media = mean(idoneidad), 
        mediana = median(idoneidad), 
        desv_est = sd(idoneidad)
    ) %>% 
    rename(tipo = tipo_id)

save(tabla_suit, suit_stats, 
    file = "datos_procesados/2019-02-14_suitabilility_report_ponca.RData")


