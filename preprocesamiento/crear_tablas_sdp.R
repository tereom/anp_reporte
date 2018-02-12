library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(dplyr)
library(stringr)
library(purrr)
library(rslurm)
library(lubridate)
library(readr)
library(ggplot2)
library(stringr)

setwd("/LUSTRE/sacmod/reportes_anp/")

# anps
path_anps_shp <- "/LUSTRE/sacmod/reportes_anp/datos_insumo/shapes_anp/anp_sinBuffer"
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"
#anillos
path_anps_shp <- "/LUSTRE/sacmod/reportes_anp/datos_insumo/shapes_anp/anp_rings"
path_anps_rings_shp <- "datos_insumo/shapes_anp/anp_rings/"

anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

anps_ring_shp <- list.files(path_anps_rings_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

# crear brick con mapas
suit_files <- list.files("datos_insumo/puma_sdp_maps", pattern = "suitability", 
    full.names = TRUE)
suit_files <- set_names(suit_files, str_c("anio_", parse_number(suit_files)))
suit_rasters <- map(suit_files, raster::raster)
suit_brick <- raster::stack(suit_rasters)

anp_suitability <- function(anp_shp, path_anps_shp){
    anp  <- rgdal::readOGR(path_anps_shp, anp_shp)
    anp_suit <- suit_brick %>% 
        raster::crop(anp) %>%            # cortamos el raster a la dimensión del ANP
        raster::mask(mask = anp)         # ponemos máscara ANP
    anp_suit_df <- data.frame(anp = anp_shp, raster::rasterToPoints(anp_suit), 
        stringsAsFactors = FALSE)
    return(anp_suit_df)
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
    "anp_terrestres_2017_NOMBRE_Tula"
    )
tabla_suit <- map_df(anps_shp[!(anps_shp %in% quitar)], 
    ~anp_suitability(., path_anps_shp))
tabla_suit_rings <- map_df(anps_ring_shp[!(anps_shp %in% quitar)], 
    ~anp_suitability(., path_anps_rings_shp))


save(tabla_suit, file = "datos_procesados/2018-02-12_suitability.RData")
save(tabla_suit_rings, file = "datos_procesados/2018-02-12_suitability_rings.RData")




