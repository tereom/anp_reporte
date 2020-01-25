# crear tabla de categorías CONANP y extents ANPs
library(tidyverse)
library(sf)


anp_eco_df <- read_rds("datos_procesados/area_ecorregion/2018-08-08_ecorregion.RData")

anp_ha <- anp_eco_df %>% 
    group_by(anp) %>% 
    mutate(
        p_area_eco = (hectareas / sum(hectareas)) * 100, 
        hectareas = first(S_TERRES)
    ) %>%
    top_n(1, p_area_eco) 

# obtener categoría de manejo de los shapes
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"
anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

names(anps_shp) <- anps_shp
manejo_anp <- map_df(anps_shp, ~st_read(str_c(path_anps_shp, "/", ., ".shp"), 
        stringsAsFactors = FALSE)$CAT_MANEJO[1]) %>% 
    gather(anp, cat_manejo)

region_anp <- map_df(anps_shp, ~st_read(str_c(path_anps_shp, "/", ., ".shp"), 
    stringsAsFactors = FALSE)$REGION[1]) %>% 
    gather(anp, region)
region_anp <- region_anp %>% 
    mutate(
        region = str_replace(region, "Dirección Regional ", ""), 
        region = ifelse(region == "Norte y Sierra Madre Occidental; Occidente y Pacífico Centro", 
            "Norte y Sierra Madre Occidental", region), 
        region = ifelse(region == "Península de Baja California y Pacífico Norte; Noroeste y Alto Golfo de California", 
            "Península de Baja California y Pacífico Norte", region)
    ) 

manejo_ha_anp <- anp_ha %>% left_join(manejo_anp) %>% left_join(region_anp)
manejo_ha_anp$tamano <- Hmisc::cut2(manejo_ha_anp$hectareas, g = 3)
glimpse(manejo_ha_anp)

# write_csv(manejo_ha_anp, "datos_procesados/2018-08-10_manejo_ha_anp.csv")

# obtener extent
extent_anp <- map_df(anps_shp, function(x){
    bbox <- st_read(str_c(path_anps_shp, "/", x, ".shp"), 
        stringsAsFactors = FALSE) %>% 
        st_bbox() 
    data.frame(xmin = bbox["xmin"], xmax = bbox["xmax"], ymin = bbox["ymin"], 
        ymax = bbox["ymax"])
    }, .id = "anp")
manejo_ha_extent_anp <- manejo_ha_anp %>% 
    left_join(extent_anp)
write_csv(manejo_ha_extent_anp, "datos_procesados/2019-01-15_manejo_ha_extent_anp.csv")


nombres <- readr::read_delim("datos_insumo/anp_nombres.tsv", "\t", 
    escape_double = FALSE, trim_ws = TRUE)
nombres_anp <- nombres %>%
    left_join(nombres_anp, by = c("anp_sin_acentos" = "anp")) %>% 
    select(id_07, anp = anp_sin_acentos, nombre)


# errores en geometrías
path_anps_shp <- "datos_insumo/shapes_anp/anp_sinBuffer"
# para anillos
# path_anps_shp <- "../datos_insumo/shapes_anp/anp_rings"

anps_shp <- list.files(path_anps_shp, 
    pattern = ".shp", recursive = FALSE) %>% 
    tools::file_path_sans_ext()

names(anps_shp) <- anps_shp
nombres_anp <- map_df(anps_shp, function(x){
    sf_anp <- st_read(str_c(path_anps_shp, "/", x, ".shp"), 
        stringsAsFactors = FALSE) %>% 
        as_data_frame() %>% 
        select(id_07 = ID_07, nombre = NOMBRE)}, .id = "anp") 

nombres_anp %>% filter(duplicated(nombres_anp$anp))
