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
setwd("/LUSTRE/sacmod/reportes_anp/procesar_ecorregion")

# lista de archivos shapes
path_anps_shp <- "/LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

# necesitamos proyectar a R^2 para calcular área
proj_lcc <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0"

# para hacer más fácil el cálculo creamos un nuevo shape que contenga únicamente
# la clasificación en 7 ecorregiones

ecorregiones <- readOGR("../datos_insumo/ecorregiones/", "ecort08gw")
ecorregiones_7_sp <- unionSpatialPolygons(ecorregiones, ecorregiones$DESECON1)
ecorregiones_7_df <- distinct(data.frame(eco = ecorregiones$DESECON1))
rownames(ecorregiones_7_df) <- ecorregiones_7_df[, 1]
ecorregiones_7 <- SpatialPolygonsDataFrame(ecorregiones_7_sp, data = ecorregiones_7_df)
ecorregiones_7 <- spTransform(ecorregiones_7, proj_lcc)

calcular_ecorregion <- function(anp_shp, path_anps_shp){
    anp <- readOGR(path_anps_shp, anp_shp, verbose = FALSE)
    anp <- spTransform(anp, crs(ecorregiones_7))
    anp_eco <- raster::intersect(gBuffer(ecorregiones_7, byid=TRUE, width=0), 
        gBuffer(anp, byid=TRUE, width=0))
    # para calcular área en m2 reproyectamos a lcc
    anp_eco@data$area <- gArea(anp_eco, byid = TRUE)
    anp_eco_df <- anp_eco@data %>% 
        dplyr::group_by(eco) %>% 
        dplyr::summarise(hectareas = sum(area) / 10000) %>% 
        dplyr::mutate(anp = anp_shp, eco = as.character(eco)) 
    anp_eco_df
}

# data frame para iterar
params_df <- dplyr::data_frame(anp_shp = anps_shp, path_anps_shp = path_anps_shp)
sjob <- slurm_apply(calcular_ecorregion, params = params_df, jobname = "ecorregion_area_job", 
    nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "5", ntasks = "5"), add_objects = "ecorregiones_7")
print_job_status(sjob)

# anp_eco_list <- purrr::map(anps_shp, ~calcular_ecorregion(., path_anps_shp))

### Read results
library(dplyr)
library(purrr)

setwd("/LUSTRE/sacmod/reportes_anp/procesar_ecorregion/_rslurm_ecorregion_area_job")

anp_eco_list <- list.files(".", "results", 
    full.names = TRUE) %>% 
    map(readRDS) %>% 
    flatten() 

anp_eco_df <- bind_rows(anp_eco_list) %>% 
    mutate(anp = stringr::str_replace(anp, "anp_terrestres_2017_NOMBRE_", ""))

save(anp_eco_df, file = "../../datos_procesados/2017-10-07_ecorregion.RData")


### correr en LUSTRE ANILLOS
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
setwd("/LUSTRE/sacmod/reportes_anp/procesar_ecorregion_anillos")

path_anps_shp <- "/LUSTRE/MADMEX/eodata/footprints/anp/anp_rings"
anps_shp <- list.files(path_anps_shp,
    pattern = ".shp", recursive = FALSE) %>%
    tools::file_path_sans_ext()

### correr función y crear ecorregiones_7

params_df <- dplyr::data_frame(anp_shp = anps_shp, path_anps_shp = path_anps_shp)
sjob <- slurm_apply(calcular_ecorregion, params = params_df, jobname = "ecorregion_area_job",
nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus",
nodes = "5", ntasks = "5"), add_objects = "ecorregiones_7")
print_job_status(sjob)

# anp_eco_list <- purrr::map(anps_shp, ~calcular_ecorregion(., path_anps_shp))

### Read results
library(dplyr)
library(purrr)

setwd("/LUSTRE/sacmod/reportes_anp/procesar_ecorregion_anillos/_rslurm_ecorregion_area_job")

anp_eco_list <- list.files(".", "results",
    full.names = TRUE) %>%
    map(readRDS) %>%
    flatten()

anp_rings_eco_df <- bind_rows(anp_eco_list) %>%
    mutate(anp = stringr::str_replace(anp, "anp_terrestres_2017_NOMBRE_", ""))

save(anp_rings_eco_df, file = "../../datos_procesados/2017-10-07_ecorregion_rings.RData")

##############################################################################
# crear tabla región CONANP
##############################################################################

calcular_region <- function(anp_shp, path_anps_shp){
    anp <- readOGR(path_anps_shp, anp_shp, verbose = FALSE)
    distinct(data_frame(region = anp$REGION, anp = anp_shp))
}

# lista de archivos shapes
path_anps_shp <- "/LUSTRE/MADMEX/eodata/footprints/anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

anp_region <- purrr::map_df(anps_shp, ~calcular_region(., path_anps_shp))
anp_region_cl <- anp_region %>% 
    mutate(
        region = str_replace(region, "Dirección Regional ", ""), 
        anp = str_replace(anp, "anp_terrestres_2017_NOMBRE_", "") 
    ) %>% 
    filter(!(stringr::str_detect(region, ";")))

save(anp_region_cl, file = "../datos_procesados/2017-10-18_anp_region.RData")
