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

anps_shp_ring <- list.files(path_anps_rings_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

puma_suit_2004 <- raster::raster("../datos_insumo/puma_sdp_maps/puma_concolor_suitability_proj_2004.tif")
anp_shp <- anps_shp[10]
suit_raster <- puma_suit_2004


# debería leer las ANPs una sola vez

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

tabla_suit <- map_df(anps_shp[c(13, 63)], ~anp_suitability(., path_anps_shp))
tabla_suit_rings <- map_df(anps_shp_ring[c(13, 63)], ~anp_suitability(., path_anps_rings_shp))


save(tabla_suit, file = "datos_procesados/2018-02-09_suitability.RData")
save(tabla_suit_rings, file = "datos_procesados/2018-02-09_suitability_rings.RData")





params_anp <- expand.grid(suit_rasters, anps_shp[c(13, 63)],
    stringsAsFactors = FALSE)

params_anp_rings <- expand.grid(suit_rasters, anps_shp[c(13, 63)],
    stringsAsFactors = FALSE)

a <- anp_suitability(suit_raster = params$Var2[1], anp_shp = params_funcion$Var1[1], 
    path_anps_shp = params$path_anps_shp[1])

tabla_suit <- params %>% 
    rename(suit_raster = Var1, anp_shp = Var2) %>% 
    group_by(suit_raster, anp_shp) %>% 
    do(suitability = anp_suitability(suit_raster = .$suit_raster, anp_shp = .$anp_shp, 
        path_anps_shp))

tabla_suit_calakmul <- tabla_suit %>% 
    mutate(anio = parse_number(suit_raster)) %>% 
    filter(stringr::str_detect(anp_shp, "Calakmul"))

tabla_suit_calakmul %>% 
    ungroup() %>% 
    group_by(anio) %>% 
    do(map_df(.$suitability, ~.))

bind_rows(!!!tabla_suit_calakmul$suitability)

ggplot(tabla_suit_2, aes(x = factor(anio), y = sims, group = anio)) +
    geom_boxplot()

