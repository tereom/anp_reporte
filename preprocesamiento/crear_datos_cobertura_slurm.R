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

setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss")

# anps
path_anps_shp <- "/LUSTRE/sacmod/reportes_anp/datos_insumo/shapes_anp/anp_sinBuffer"
#anillos
path_anps_shp <- "/LUSTRE/sacmod/reportes_anp/datos_insumo/shapes_anp/anp_rings"

anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

hansen_loss <- raster::raster("../datos_insumo/hansen_forest_loss_v1_4_wgs84nodefs.tif")
madmex_class <- raster::raster("../datos_insumo/madmex_nalc_10c_30m_2010.tif")

perdida <- function(anp_shp, path_anps_shp){
    anp  <- rgdal::readOGR(path_anps_shp, anp_shp)
    anp_loss <- hansen_loss %>% 
        raster::crop(anp) %>%            # cortamos el raster a la dimensión del ANP
        raster::mask(mask = anp)         # ponemos máscara ANP
    anp_madmex <- madmex_class %>% 
        raster::crop(anp) %>%            # cortamos el raster a la dimensión del ANP
        raster::mask(mask = anp) %>% 
        raster::projectRaster(anp_loss, method = "ngb")

    # stack rasters
    anp_madmex_loss <- raster::stack(list(anp_loss, anp_madmex)) # hacemos stack para convertir a df
    
    # now you can convert this to a matrix or data.frame
    anp_madmex_loss_df <- data.frame(anp = anp_shp, raster::rasterToPoints(anp_madmex_loss), 
        stringsAsFactors = FALSE) %>% 
        dplyr::rename(year_loss = hansen_forest_loss_v1_4_wgs84nodefs, 
            clase_madmex = madmex_nalc_10c_30m_2010) %>% 
        dplyr::count(year_loss, clase_madmex) %>% 
        dplyr::mutate(anp = anp_shp) %>% 
        ungroup()
}

# parámetros en data.frame para la llamada de slurm_apply
params_df <- data.frame(anp_shp = anps_shp, path_anps_shp = path_anps_shp, 
    stringsAsFactors = FALSE)
sjob <- slurm_apply(perdida, params = params_df, jobname = "forest_loss_job_rings_2", 
    nodes = 4, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "4", ntasks = "4"), add_objects = c("hansen_loss", "madmex_class"))
print_job_status(sjob)

### Read results
setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss/_rslurm_forest_loss_job_2/")
setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss/_rslurm_forest_loss_job_rings_2/")

anp_loss_list <- list.files(".", "results", 
    full.names = TRUE) %>% 
    map(readRDS) %>% 
    flatten()

perdida_anps_df <- bind_rows(anp_loss_list)
perdida_anps_rings_df <- bind_rows(anp_loss_list)

save(perdida_anps_df, file = "../../datos_procesados/2017-10-22_perdida_cobertura.RData")
save(perdida_anps_rings_df, file = "../../datos_procesados/2017-10-22_perdida_cobertura_rings.RData")
