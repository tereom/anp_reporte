library(raster)
library(dplyr)
library(stringr)
library(purrr)
library(rslurm)
library(lubridate)
library(sf)

# calcular pérdida en hectáreas y por cobertura madmex

hansen_loss <- raster::raster("rasters/hansen_forest_loss_v1_5_wgs84nodefs_cropped.tif")
madmex_class <- raster::raster("rasters/mapabase_8clases_re_2015_30m_wgs84nodefs_cropped.tif")

madmex_loss <- raster::stack(list(hansen_loss, madmex_class)) 

perdida <- function(anp_shp){
    anp  <- sf::st_read(anp_shp, stringsAsFactors = FALSE) %>% distinct() 
    anp_madmex_loss <- madmex_loss %>% 
        raster::crop(anp) %>% 
        raster::mask(mask = anp) 
    anp_madmex_loss_df <- data.frame( 
        raster::rasterToPoints(anp_madmex_loss), stringsAsFactors = FALSE) %>% 
        dplyr::rename(year_loss = hansen_forest_loss_v1_5_wgs84nodefs_cropped, 
            clase_madmex = mapabase_8clases_re_2015_30m_wgs84nodefs_cropped) %>% 
        dplyr::count(year_loss, clase_madmex) %>% 
        ungroup() %>% 
        dplyr::mutate(
            anp = basename(tools::file_path_sans_ext(anp_shp)),
            anp = str_replace(anp, pattern = "_ring", ""), 
            year_loss = year_loss + 2000
            )
}

# anps
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = TRUE)
cobertura <- map(anps_shp[1:5], perdida)


# en slurm


perdida <- function(anp_shp, path_anps_shp){
    anp  <- rgdal::readOGR(path_anps_shp, anp_shp)
    anp_madmex_loss <- madmex_loss %>% 
        raster::crop(anp) %>% 
        raster::mask(mask = anp) 
    anp_madmex_loss_df <- data.frame( 
        raster::rasterToPoints(anp_madmex_loss), stringsAsFactors = FALSE) %>% 
        dplyr::rename(year_loss = hansen_forest_loss_v1_5_wgs84nodefs_cropped, 
            clase_madmex = mapabase_8clases_re_2015_30m_wgs84nodefs_cropped) %>% 
        dplyr::count(year_loss, clase_madmex) %>% 
        ungroup() %>% 
        dplyr::mutate(
            anp = basename(tools::file_path_sans_ext(anp_shp)),
            anp = str_replace(anp, pattern = "_ring", ""), 
            year_loss = year_loss + 2000
        )
}

setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss")

hansen_loss <- raster::raster("../datos_insumo/rasters_reportes_conanp/hansen_forest_loss_v1_5_wgs84nodefs_cropped.tif")
madmex_class <- raster::raster("../datos_insumo/rasters_reportes_conanp/mapabase_8clases_re_2015_30m_wgs84nodefs_cropped.tif")

madmex_loss <- raster::stack(list(hansen_loss, madmex_class)) 


path_anps_shp <- "/LUSTRE/sacmod/reportes_anp/datos_insumo/shapes_anp/anp_sinBuffer"
path_anps_shp <- "/LUSTRE/sacmod/reportes_anp/datos_insumo/shapes_anp/anp_rings"
path_anps_shp <- "/LUSTRE/sacmod/reportes_anp/datos_insumo/shapes_anp/anp_zonasnucleo"
path_anps_shp <- "/LUSTRE/sacmod/reportes_anp/datos_insumo/shapes_anp/anp_zonaspreservacion"

anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
    full.names = FALSE) %>% 
    tools::file_path_sans_ext()

# parámetros en data.frame para la llamada de slurm_apply
params_df <- data.frame(anp_shp = anps_shp, path_anps_shp = path_anps_shp, 
    stringsAsFactors = FALSE)
sjob <- slurm_apply(perdida, params = params_df, jobname = "forest_loss_preservacion", 
    nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
    nodes = "4", ntasks = "4"), add_objects = c("madmex_loss"))
print_job_status(sjob)

### Read results
setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss/_rslurm_forest_loss/")
setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss/_rslurm_forest_loss_rings/")
setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss/_rslurm_forest_loss_nucleo/")
setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss/_rslurm_forest_loss_preservacion/")


anp_loss_list <- list.files(".", "results", 
    full.names = TRUE) %>% 
    map(readRDS) %>% 
    flatten()

perdida_anps_df <- bind_rows(anp_loss_list)
perdida_anps_rings_df <- bind_rows(anp_loss_list)
perdida_anps_zn_df <- bind_rows(anp_loss_list)
perdida_anps_zp_df <- bind_rows(anp_loss_list)

save(perdida_anps_df, file = "../../datos_procesados/2018-09-05_perdida_cobertura.RData")
save(perdida_anps_rings_df, file = "../../datos_procesados/2018-09-05_perdida_cobertura_rings.RData")
save(perdida_anps_zn_df, file = "../../datos_procesados/2018-09-05_perdida_cobertura_zn.RData")
save(perdida_anps_zp_df, file = "../../datos_procesados/2018-09-05_perdida_cobertura_zp.RData")

