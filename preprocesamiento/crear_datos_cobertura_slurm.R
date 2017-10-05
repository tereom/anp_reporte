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

path_anps_shp <- "/LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer"

anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

hansen_loss <- raster("/LUSTRE/sacmod/reportes_anp/datos/hansen_forest_loss_v1_3.tif")
madmex_class <- raster("/LUSTRE/sacmod/reportes_anp/datos_procesados/madmex_nalc_10c_30m_2010.tif")

perdida <- function(anp_shp, path_anps_shp){
    anp  <- readOGR(path_anps_shp, anp_shp)
    anp_loss <- hansen_loss %>% 
        crop(anp) %>%            # cortamos el raster a la dimensión del ANP
        mask(mask = anp)         # ponemos máscara ANP
    anp_madmex <- madmex_class %>% 
        crop(anp) %>%            # cortamos el raster a la dimensión del ANP
        mask(mask = anp) %>%     # ponemos máscara ANP
        projectRaster(anp_loss, method = "ngb") # reproyectamos para que sean iguales
    
    # stack rasters
    anp_madmex_loss <- stack(list(anp_loss, anp_madmex)) # hacemos stack para convertir a df
    
    # now you can convert this to a matrix or data.frame
    anp_madmex_loss_df <- data.frame(anp = anp_shp, rasterToPoints(anp_madmex_loss)) %>% 
        rename(year_loss = hansen_forest_loss_v1_3, clase_madmex = madmex_nalc_10c_30m_2010) %>% 
        count(year_loss, clase_madmex) %>% 
        mutate(anp = anp_shp) %>% 
        ungroup()
}

# parámetros en data.frame para la llamada de slurm_apply
params_df <- dplyr::data_frame(anp_shp = anps_shp, path_anps_shp = path_anps_shp)
sjob <- slurm_apply(perdida, params = params_df, jobname = "forest_loss_job", 
    nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "5", ntasks = "3"), add_objects = c("hansen_loss", "madmex_class"))
print_job_status(sjob)

### Read results
anp_loss_list <- list.files(".", "results", 
    full.names = TRUE) %>% 
    map(readRDS) %>% 
    flatten()

# esto no hará falta con la nueva función, se conserva el orden??!!!
anp_loss_list_named <- anp_loss_list %>% map2(anps_shp, 
    function(x, y){
        x$anp <- y
        x
    })

perdida_anps_df <- bind_rows(anp_loss_list_named)

save(perdida_anps_df, file = "../../datos/2017-10-02_perdida_cobertura.RData")


##### Correr en los anillos
setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss")

path_anps_rings_shp <- "/LUSTRE/MADMEX/eodata/footprints/anp/anp_rings"
path_anps_shp <- "/LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()
hansen_loss <- raster("/LUSTRE/sacmod/reportes_anp/datos/hansen_forest_loss_v1_3.tif")
madmex_class <- raster("/LUSTRE/sacmod/reportes_anp/datos_procesados/madmex_nalc_10c_30m_2010.tif")

params_df <- dplyr::data_frame(anp_shp = stringr::str_c(anps_shp, "_ring"), 
    path_anps_shp = path_anps_rings_shp)
sjob <- slurm_apply(perdida, params = params_df, jobname = "forest_loss_rings_job", 
    nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "5", ntasks = "3"), add_objects = c("hansen_loss", "madmex_class"))

##### Leer resultados
library(purrr)
library(tidyverse)
library(stringr)
library(plotly)

setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss/_rslurm_forest_loss_rings_job/")

anp_loss__rings_list <- list.files(".", "results", 
    full.names = TRUE) %>% 
    map(readRDS) %>% 
    flatten()

perdida_anps_rings_df <- bind_rows(anp_loss__rings_list)

save(perdida_anps_rings_df, file = "../../datos/2017-10-04_perdida_cobertura_rings.RData")

