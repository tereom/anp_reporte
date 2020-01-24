# corre en cluster y en local
# 
# entrada: rasters hansen (2018), y madmex 2015
#          shapes anps, rings, zonasnucleo, zonaspreservacion
# salidas: 
#   1. aaaa_mm_dd_anp_tipo_loss.RData donde tipo es sinBuffer, rings, 
#       zonasnucleo zonaspreservacion: pérdida en pixeles por año.
#   2. aaaa_mm_dd_anp_tipo_madmex.RData donde tipo es sinBuffer, rings, 
#       zonasnucleo, zonaspreservacion: número de pixeles en cada clase Madmex.  
#   3. aaaa_mm_dd_anp_tipo_madmex_loss.RData donde tipo es sinBuffer, rings, 
#       zonasnucleo, zonaspreservacion: pérdida en pixeles por año para cada 
#       clase Madmex.

library(raster)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(sf)

# calcular pérdida en hectáreas y por cobertura madmex (local es lento ver 
# opción de cluster)

hansen_loss <- raster::raster("datos_procesados/rasters/hansen_forest_loss_v1_6_lcc_cropped.tif")
madmex_class <-  raster::raster("datos_procesados/rasters/mapabase_8clases_re_2015_30m_lcc_cropped.tif")

madmex_loss <- raster::stack(list(hansen_loss, madmex_class)) 
crs_loss <- as.character(crs(madmex_loss))
perdida <- function(anp_shp){
    anp  <- sf::st_read(anp_shp, stringsAsFactors = FALSE) %>% 
        distinct() %>% 
        sf::st_transform(crs_loss)
    anp_madmex_loss <- madmex_loss  %>% 
        raster::crop(anp) %>% 
        raster::mask(mask = anp) 
    anp_madmex_loss_df <- data.frame( 
        raster::rasterToPoints(anp_madmex_loss), stringsAsFactors = FALSE) %>% 
        dplyr::rename(year_loss = hansen_forest_loss_v1_6_lcc_cropped, 
            clase_madmex = mapabase_8clases_re_2015_30m_lcc_cropped) %>% 
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
cobertura <- map(anps_shp, perdida)

############################# CLUSTER
# con slurm
library(dplyr)
library(purrr)
library(rslurm)
setwd("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss")

hansen_loss <- raster::raster("../datos_insumo/rasters_reportes_conanp/hansen_forest_loss_v1_6_lcc_cropped.tif")
madmex_class <- raster::raster("../datos_insumo/rasters_reportes_conanp/mapabase_8clases_re_2015_30m_lcc_cropped.tif")

madmex_loss <- raster::stack(list(hansen_loss, madmex_class)) 
crs_loss <- raster::crs(madmex_loss)

perdida_clase <- function(anp_shp, path_anp_shp){
    anp  <- rgdal::readOGR(path_anp_shp, anp_shp) %>% 
        sp::spTransform("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    try(anp_madmex_loss <- madmex_loss %>% 
            raster::crop(anp) %>% 
            raster::mask(mask = anp))
    if (!is.null(anp_madmex_loss)) {
        try(anp_madmex_loss_df <- data.frame( 
            raster::rasterToPoints(anp_madmex_loss), stringsAsFactors = FALSE) %>% 
                dplyr::rename(year_loss = hansen_forest_loss_v1_6_lcc_cropped, 
                    clase_madmex = mapabase_8clases_re_2015_30m_lcc_cropped) %>% 
                dplyr::count(year_loss, clase_madmex) %>% 
                ungroup() %>% 
                dplyr::mutate(
                    anp = basename(tools::file_path_sans_ext(anp_shp)),
                    anp = stringr::str_replace(anp, pattern = "_ring", ""), 
                    year_loss = year_loss + 2000
                )
        )
        if (is.null(anp_madmex_loss) | is.null(anp_madmex_loss_df)) {
            return(NA)
        }
        return(anp_madmex_loss_df)
        
    }
    
}

perdida <- function(anp_shp, path_anp_shp){
    anp  <- rgdal::readOGR(path_anp_shp, anp_shp) %>% 
        sp::spTransform("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    anp_hansen_loss <- hansen_loss %>% 
        raster::crop(anp) %>% 
        raster::mask(mask = anp) 
    anp_madmex_loss_df <- data.frame( 
        raster::rasterToPoints(anp_hansen_loss), stringsAsFactors = FALSE) %>% 
        dplyr::rename(year_loss = hansen_forest_loss_v1_6_lcc_cropped) %>% 
        dplyr::count(year_loss) %>% 
        ungroup() %>% 
        dplyr::mutate(
            anp = basename(tools::file_path_sans_ext(anp_shp)),
            anp = stringr::str_replace(anp, pattern = "_ring", ""), 
            year_loss = year_loss + 2000
        )
}

clase <- function(anp_shp, path_anp_shp){
    anp  <- rgdal::readOGR(path_anp_shp, anp_shp) %>% 
        sp::spTransform("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    anp_madmex_class <- madmex_class %>% 
        raster::crop(anp) %>% 
        raster::mask(mask = anp) 
    anp_madmex_class_df <- data.frame( 
        raster::rasterToPoints(anp_madmex_class), stringsAsFactors = FALSE) %>% 
        dplyr::rename(clase_madmex = mapabase_8clases_re_2015_30m_lcc_cropped) %>% 
        dplyr::count(clase_madmex) %>% 
        ungroup() %>% 
        dplyr::mutate(
            anp = basename(tools::file_path_sans_ext(anp_shp)),
            anp = stringr::str_replace(anp, pattern = "_ring", "")
        )
}

correr_slurm_perdida <- function(tipo = "anp_sinBuffer"){
    path_anps_shp <- paste0("/LUSTRE/sacmod/reportes_anp/datos_insumo/shapes_anp/", 
        tipo)
    anps_shp <- list.files(path_anps_shp, pattern = ".shp", recursive = FALSE, 
        full.names = FALSE) %>% 
        tools::file_path_sans_ext()
    # parámetros en data.frame para la llamada de slurm_apply
    params_df <- data.frame(anp_shp = anps_shp, path_anp_shp = path_anps_shp, 
        stringsAsFactors = FALSE)
    sjob_madmex_loss <- slurm_apply(perdida_clase, params = params_df,
        jobname = paste0("madmex_loss_2019_", tipo),
        nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus",
            nodes = "4", ntasks = "4"), add_objects = c("madmex_loss"))
    sjob_madmex <- slurm_apply(clase, params = params_df,
        jobname = paste0("madmex_2019_", tipo),
        nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus",
            nodes = "4", ntasks = "4"), add_objects = c("madmex_class"))
    sjob_loss <- slurm_apply(perdida, params = params_df,
        jobname = paste0("hansen_loss_2019_", tipo),
        nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus",
            nodes = "4", ntasks = "4"), add_objects = c("hansen_loss"))
}

correr_slurm_perdida("anp_sinBuffer")
correr_slurm_perdida("anp_rings")
correr_slurm_perdida("anp_zonasnucleo")
correr_slurm_perdida("anp_zonaspreservacion")

#############################  En local

escribir_resultados <- function(tipo, tipo_valor){
    # ruta_cobertura <- paste0("/LUSTRE/sacmod/reportes_anp/procesar_forest_loss/_rslurm_",
    #   tipo_valor, "_2019_", tipo)
    ruta_cobertura <- paste0("/Volumes/sacmod/reportes_anp/procesar_forest_loss/_rslurm_", 
        tipo_valor, "_2019_", tipo)
    anp_cobertura_list <- list.files(ruta_cobertura, "results", full.names = TRUE) %>% 
        purrr::map(read_rds) %>% 
        purrr::flatten()
    is_df <- map_lgl(anp_cobertura_list, is.data.frame)
    anp_cobertura_list[is_df] %>% 
        bind_rows() %>% 
        # write_rds(path = paste0("/LUSTRE/sacmod/reportes_anp/datos_procesados/",
        #     "2019-05-09_", tipo, "_", tipo_valor, ".RData"))
        write_rds(path = paste0("datos_procesados/madmex_hansen/",
            "2019-05-09_", tipo, "_", tipo_valor, ".RData"))
    if (sum(!is_df) > 0) {
        names(anp_cobertura_list[!is_df]) %>% 
            # write.table(file = paste0("/LUSTRE/sacmod/reportes_anp/datos_procesados/",
            #     "2019-05-09_errores_", tipo, "_", tipo_valor, ".csv"), 
            #     col.names = "anp", row.names = FALSE, sep = ",")   
            write.table(file = paste0("datos_procesados/madmex_hansen/",
                "2019-05-09_errores_", tipo, "_", tipo_valor, ".csv"),
                col.names = "anp", row.names = FALSE, sep = ",")
    }
}

tipo_valor <- c("madmex", "madmex_loss", "hansen_loss")
tipo <- c("anp_sinBuffer", "anp_rings", "anp_zonasnucleo", "anp_zonaspreservacion")
tipos <- expand.grid(tipo, tipo_valor, stringsAsFactors = FALSE) %>% 
    rename(tipo = Var1, tipo_valor = Var2)

walk(tipo_valor, ~escribir_resultados(tipo = "anp_sinBuffer", .))
walk(tipo_valor, ~escribir_resultados(tipo = "anp_rings", .))
walk(tipo_valor, ~escribir_resultados(tipo = "anp_zonasnucleo", .))
walk(tipo_valor, ~escribir_resultados(tipo = "anp_zonaspreservacion", .))


