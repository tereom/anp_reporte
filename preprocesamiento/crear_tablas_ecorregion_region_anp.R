### correr en LUSTRE
library(raster)
library(rslurm)
library(dplyr)
library(stringr)
library(purrr)
library(maptools)
library(sf)

# usaremos clasificación en 7 ecorregiones
ecorregiones <- st_read("datos_insumo/ecorregiones/ecort08gw.shp", 
    stringsAsFactors = FALSE)
ecorregiones_7 <- ecorregiones %>% 
    group_by(DESECON1) %>% 
    summarise(CVEECON1 = first(CVEECON1))
# proyectar para calcular área
ecorregiones_lcc <- st_transform(ecorregiones_7, 6362)

# calcular área intersectando con ecorregiones y agregar info de área que esta 
# en shape: S_TERRES y SUPERFICIE

calcular_ecorregion_anp <- function(anp_shp){
    print(anp_shp)
    anp <- st_read(anp_shp, stringsAsFactors = FALSE) %>% distinct() 
    if (str_detect(anp_shp, "Islas_del_Pacifico_de_la_Peninsula_de_Baja")) {
        anp <- anp[1, ]
    }
    anp_lcc <- st_transform(anp, 6362)
    anp_eco <- st_intersection(anp_lcc, ecorregiones_lcc)
    anp_eco$area_r <- st_area(anp_eco)
    anp_eco_df <- anp_eco %>% 
        as_data_frame() %>% 
        group_by(DESECON1, ID_07, NOMBRE) %>% 
        summarise(SUPERFICIE = mean(SUPERFICIE), S_TERRES = mean(S_TERRES), 
            hectareas = as.numeric(sum(area_r) / 10000)) %>% 
        mutate(anp = tools::file_path_sans_ext(basename(anp_shp))) 
    return(anp_eco_df)
}

# enlistamos archivos de ANPs
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
# calculamos áreas para cada ANP
anps_eco_area <- map(anps_shp, calcular_ecorregion_anp)
anp_eco_df <- bind_rows(!!!anps_eco_area) %>% 
    rename(eco = DESECON1, id_07 = ID_07)

# save(anp_eco_df, file = "datos_procesados/2018-08-08_ecorregion.RData")

# en el caso de anillos, zonas núcleo y zonas de preservación no tenemos 
# información de área en el shape y no nos interesa la ecorregión en que caen
# por lo que usamos una función más sencilla
calcular_area <- function(anp_shp){
    print(anp_shp)
    anp <- st_read(anp_shp, stringsAsFactors = FALSE) %>% distinct() 
    anp_lcc <- st_transform(anp, 6362)
    anp_lcc$area_r <- st_area(anp_lcc)
    anp_area <- anp_lcc %>% 
        as_data_frame() %>% 
        mutate(
            anp = tools::file_path_sans_ext(basename(anp_shp)), 
            hectareas = as.numeric(sum(area_r) / 10000)
            ) %>% 
        select(id_07 = ID_07, anp, hectareas)
    return(anp_area)
}

# lista de archivos shapes
path_anps_shp <- "datos_insumo/shapes_anp/anp_rings"
path_anps_shp <- "datos_insumo/shapes_anp/anp_zonasnucleo"
path_anps_shp <- "datos_insumo/shapes_anp/anp_zonaspreservacion/"

anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
anps_area <- map(anps_shp, calcular_area)
anps_area_anillos <- bind_rows(!!!anps_area)
anps_area_zonasnucleo <- bind_rows(!!!anps_area)

# en zonas de preservación falla función calcular_area (en part. el distinct) 
# para ANP Constitución

calcular_area_2 <- function(anp_shp){
    print(anp_shp)
    anp <- st_read(anp_shp, stringsAsFactors = FALSE)
    anp_lcc <- st_transform(anp, 6362)
    anp_lcc$area_r <- st_area(anp_lcc)
    anp_area <- anp_lcc %>% 
        as_data_frame() %>% 
        mutate(
            anp = tools::file_path_sans_ext(basename(anp_shp)), 
            hectareas = as.numeric(sum(area_r) / 10000)
        ) %>% 
        select(id_07 = ID_07, anp, hectareas)
    return(anp_area)
}
anps_area <- map(anps_shp, calcular_area_2)
anps_area_zonaspreservacion <- bind_rows(!!!anps_area)

save(anps_area_anillos, file = "datos_procesados/2018-08-08_area_rings.RData")
save(anps_area_zonasnucleo, 
    file = "datos_procesados/2018-08-08_area_zonasnucleo.RData")
save(anps_area_zonaspreservacion, 
    file = "datos_procesados/2018-08-08_area_zonaspreservacion.RData")

# Notas
# * Las islas no tienen sentido
# * Hay ANPs con polígonos sobrelapados
