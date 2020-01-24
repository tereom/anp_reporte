### correr en local
library(raster)
library(dplyr)
library(stringr)
library(purrr)
library(sf)

# ie <- raster("../datos_insumo/ie_2014_250m.tif")
# ie_longlat <- projectRaster(ie, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# rf <- writeRaster(ie_longlat, filename = "../datos_procesados/2017-10-12_ie_longlat.tif",
#     format = "GTiff", overwrite = TRUE, dataType = "INT2U", options = "COMPRESS=LZW")

# raster de IE reproyectado a long-lat
ie_longlat <- raster("datos_procesados/2017-10-12_ie_longlat.tif")
    
calcular_ie <- function(anp_shp){
    anp  <- st_read(anp_shp, stringsAsFactors = FALSE) %>% distinct() 
    anp_ie <- ie_longlat %>% 
        crop(anp) %>%
        mask(mask = anp)
    anp_ie_v <- na.omit(values(anp_ie))
    anp_ie_v <- anp_ie_v[anp_ie_v >= 0]
    list(anp = tools::file_path_sans_ext(basename(anp_shp)), 
        media = mean(anp_ie_v), mediana = median(anp_ie_v), 
        desv.est = sd(anp_ie_v), 
        valores = round(sample(anp_ie_v, min(length(anp_ie_v), 1000)), 3))
}

calcular_ie_zp <- function(anp_shp){
    # misma función sin distinct para zonas de preservación
    anp  <- st_read(anp_shp, stringsAsFactors = FALSE)
    anp_ie <- ie_longlat %>% 
        crop(anp) %>%
        mask(mask = anp)
    anp_ie_v <- na.omit(values(anp_ie))
    anp_ie_v <- anp_ie_v[anp_ie_v >= 0]
    list(anp = tools::file_path_sans_ext(basename(anp_shp)), 
        media = mean(anp_ie_v), mediana = median(anp_ie_v), 
        desv.est = sd(anp_ie_v), 
        valores = round(sample(anp_ie_v, min(length(anp_ie_v), 1000)), 3))
}

# anps
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
ie_list <- purrr::map(anps_shp, calcular_ie)

# anillos
path_anps_shp <- "datos_insumo/shapes_anp/anp_rings/"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
ie_anillos_list <- purrr::map(anps_shp, calcular_ie)

# zonas núcleo
path_anps_shp <- "datos_insumo/shapes_anp/anp_zonasnucleo/"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
ie_zn_list <- purrr::map(anps_shp, calcular_ie)

# zonas preservación (misma función sin distinct)
path_anps_shp <- "datos_insumo/shapes_anp/anp_zonaspreservacion/"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
ie_zp_list <- purrr::map(anps_shp, calcular_ie_zp)

save(ie_list, ie_anillos_list, ie_zn_list, ie_zp_list, 
    file =  "datos_procesados/2018-09-04_ie_lists.RData")
    
#### Información en csv (limpiar Rdatas)
# originalmente este código estaba en llenar_db.R

library(tidyverse)
load("datos_procesados/2018-09-04_ie_lists.RData")
nombres_anp <- read_csv("datos_procesados/nombres_anps_id.csv")
anps_ids <- read_csv("datos_procesados/anps_ids.csv")


extrae_df <- function(ie_list_type, tipo){
    ie_stats <- map(ie_list_type, function(x){ x$valores <- NULL; x})
    ie_df <- bind_rows(!!!ie_stats) %>%
        mutate(
            tipo_id = tipo, 
            anp = str_replace(anp, pattern = "_ring", "")
        ) %>%
        left_join(nombres_anp, by = "anp") %>%
        left_join(anps_ids, by = c("tipo_id", "id_07", "nombre")) %>%
        select(anp_tipo_id, anp, nombre, id_07, tipo_id, 
            media, mediana, desv_est = desv.est) %>%
        na.omit()
}

# el 1:4 son los códigos de tipo de polígono (anp, anillo, núcleo,...)
ie_stats_list <- map2(list(ie_list, ie_anillos_list, ie_zn_list, ie_zp_list), 
    1:4, extrae_df)

ie_df <- bind_rows(!!!ie_stats_list) 
write_csv(ie_df, path = "datos_procesados/2018-07-27_ie_stats.csv")

# integridad ie_list_valores    
extrae_valores <- function(ie_list_type, tipo){
    ie_valores <- map_df(ie_list_type, function(x){ tibble(anp = x$anp, 
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
