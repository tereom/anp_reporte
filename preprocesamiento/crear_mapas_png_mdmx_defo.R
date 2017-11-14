################### con leaflet
library(raster)
library(mapview)
library(rgdal)
library(stringr)
library(dplyr)
library(leaflet)
library(mapview)
library(tidyverse)

madmex <- raster("datos_insumo/madmex_nalc_10c_30m_2010.tif")
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"

mapa_madmex <- function(mi_anp){
    print(mi_anp)
    mi_anp_shp_file <- list.files(path_anps_shp, 
        pattern = str_c(mi_anp, ".shp"), recursive = FALSE) %>%
        tools::file_path_sans_ext() 

    mi_anp_shp <- readOGR(path_anps_shp, mi_anp_shp_file, verbose = FALSE)

    madmex_mi_anp <- madmex %>% 
        raster::crop(mi_anp_shp) %>%
        mask(mask = mi_anp_shp)

    ext <- raster::extent(mi_anp_shp)
    colores_madmex <- c('#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', 
        '#ff7f00', '#cab2d6', '#6a3d9a', '#a6cee3', '#1f78b4')
    pal <- colorFactor(palette = colores_madmex, domain = 1:10, 
        na.color = "transparent")

    m <- leaflet(data = mi_anp_shp, options = leafletOptions(zoomControl = FALSE)) %>% 
        addPolygons(color = "#eef3f4", weight = 1.5, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.1) %>% 
        addTiles() %>%
        fitBounds(lng1 = ext@xmin, lat1 = ext@ymin, lng2 = ext@xmax, lat2 = ext@ymax) %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>% 
        addRasterImage(madmex_mi_anp, color = pal, opacity = 0.6, 
            maxBytes = 8 * 1024 * 1024) %>% 
        addLegend("bottomright", values = 1:10, colors = colores_madmex,
            title = "MAD-MEX (2010)", labels = c("bosques", "selvas", "matorrales", 
                "pastizal", "suelo desnudo", "humedal", "agricultura", 
                "asentamiento humano", "agua", "nieve y hielo"), opacity = 0.6)
    out_name = paste0(getwd(), "/datos_procesados/mapas/cobertura/", mi_anp,"_defo.png")
    mapshot(m, file = out_name)
    return(mi_anp)
}
anp_nombres <- read_delim("datos_insumo/anp_nombres.tsv", "\t", 
    escape_double = FALSE, trim_ws = TRUE)
quitar <- c("anp_terrestres_2017_NOMBRE_C.A.D.N.R._043_Estado_de_Nayarit", 
    "anp_terrestres_2017_NOMBRE_Islas_del_Golfo_de_California")

map(anp_nombres_cl$anp_sin_acentos, mapa_madmex)


### mapa hansen

hansen <- raster("datos_insumo/hansen_forest_loss_v1_4_wgs84nodefs.tif")
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"

mapa_hansen <- function(mi_anp){
    print(mi_anp)
    mi_anp_shp_file <- list.files(path_anps_shp, 
        pattern = str_c(mi_anp, ".shp"), recursive = FALSE) %>%
        tools::file_path_sans_ext() 
    
    mi_anp_shp <- readOGR(path_anps_shp, mi_anp_shp_file, verbose = FALSE)
    
    hansen_mi_anp <- hansen %>% 
        raster::crop(mi_anp_shp) %>%
        mask(mask = mi_anp_shp)
    hansen_mi_anp[hansen_mi_anp < 11] <- NA
    ext <- raster::extent(mi_anp_shp)

    pal <- colorFactor(palette = "OrRd", domain = 11:16, 
        na.color = "transparent")
    
    m <- leaflet(data = mi_anp_shp, options = leafletOptions(zoomControl = FALSE)) %>% 
        addPolygons(color = "#eef3f4", weight = 1.5, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.1) %>% 
        addTiles() %>%
        fitBounds(lng1 = ext@xmin, lat1 = ext@ymin, lng2 = ext@xmax, lat2 = ext@ymax) %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>% 
        addRasterImage(hansen_mi_anp, color = pal, opacity = 0.6, 
            maxBytes = 8 * 1024 * 1024) %>% 
        addLegend("bottomright", values = 11:16, pal = pal,
            title = "Pérdida", labels = as.character(2011:1016), opacity = 0.6)
    
    out_name = paste0(getwd(), "/datos_procesados/mapas/deforestacion/", mi_anp,"_defo.png")
    mapshot(m, file = out_name)
    return(mi_anp)
}
map(anp_nombres_cl$anp_sin_acentos, mapa_hansen)

# revisión de creados
mapas_hansen_generados <- list.files("datos_procesados/mapas/deforestacion/") %>% 
    tools::file_path_sans_ext() %>% 
    stringr::str_replace("_defo", "")

anps_faltantes <- anp_nombres_cl %>% 
    filter(!(anp_sin_acentos %in% mapas_hansen_generados)) %>% 
    pull(anp_sin_acentos)

map(anps_faltantes, mapa_hansen)
