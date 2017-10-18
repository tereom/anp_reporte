### correr en LUSTRE
library(raster)
library(rslurm)
library(rgdal)
library(sp)
library(rgeos)
library(dplyr)
library(stringr)
library(purrr)
library(maptools)

# para correr en LUSTRE ponemos directorio de trabajo
setwd("/LUSTRE/sacmod/reportes_anp/procesar_ie")

# ie <- raster("../datos_insumo/ie_2014_250m.tif")
# ie_longlat <- projectRaster(ie, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# rf <- writeRaster(ie_longlat, filename = "../datos_procesados/2017-10-12_ie_longlat.tif",
#     format = "GTiff", overwrite = TRUE, dataType = "INT2U", options = "COMPRESS=LZW")

# raster de IE reproyectado a long-lat
ie_longlat <- raster("../datos_procesados/2017-10-12_ie_longlat.tif")
    
calcular_ie <- function(anp_shp, path_anps_shp){
    anp <- readOGR(path_anps_shp, anp_shp, verbose = FALSE) 
    anp_ie <- ie_longlat %>% 
        crop(anp) %>%            # cortamos el raster a la dimensión del ANP
        mask(mask = anp)         # ponemos máscara ANP
    
    anp_ie_v <- na.omit(values(anp_ie))
    anp_ie_v <- anp_ie_v[anp_ie_v >= 0]
    list(anp = anp_shp, media = mean(anp_ie_v), mediana = median(anp_ie_v), 
        desv.est = sd(anp_ie_v), 
        valores = sample(anp_ie_v, min(length(anp_ie_v), 1000)))
}

# lista de archivos shapes
path_anps_shp <- "/LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

ie_list <- purrr::map(anps_shp, ~calcular_ie(., path_anps_shp))


path_anps_shp <- "/LUSTRE/MADMEX/eodata/footprints/anp/anp_rings"
anps_shp <- list.files(path_anps_shp,
    pattern = ".shp", recursive = FALSE) %>%
    tools::file_path_sans_ext()

ie_anillos_list <- purrr::map(anps_shp, ~calcular_ie(., path_anps_shp))

save(ie_list, ie_anillos_list,file =  "../datos_procesados/2017-10-12_ie_list.RData")


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
