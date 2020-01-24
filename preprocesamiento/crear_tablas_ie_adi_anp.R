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

# zonas preservación
path_anps_shp <- "datos_insumo/shapes_anp/anp_zonaspreservacion/"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
ie_zp_list <- purrr::map(anps_shp, calcular_ie)

save(ie_list, ie_anillos_list, ie_zn_list, ie_zp_list, 
    file =  "datos_procesados/2018-09-04.RData")


### procesar región CONANP

adi <- raster("../datos_insumo/adi/ADI_2014-2015_1000m.tif")
adi_dia <- raster("../datos_insumo/adi/ADI_dia_2014-2015_1000m.tif")
adi_noche <- raster("../datos_insumo/adi/ADI_noche_2014-2015_1000m.tif")

adi_longlat <- projectRaster(adi, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rf <- writeRaster(adi_longlat, filename = "../datos_procesados/2017-10-17_adi_longlat.tif",
    format = "GTiff", overwrite = TRUE, dataType = "INT2U", options = "COMPRESS=LZW")
adi_dia_longlat <- projectRaster(adi_dia, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rf <- writeRaster(adi_dia_longlat, filename = "../datos_procesados/2017-10-17_adi_dia_longlat.tif",
    format = "GTiff", overwrite = TRUE, dataType = "INT2U", options = "COMPRESS=LZW")
adi_noche_longlat <- projectRaster(adi_noche, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rf <- writeRaster(adi_noche_longlat, filename = "../datos_procesados/2017-10-17_adi_noche_longlat.tif",
    format = "GTiff", overwrite = TRUE, dataType = "INT2U", options = "COMPRESS=LZW")
