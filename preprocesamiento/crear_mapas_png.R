################### con leaflet
library(mapview)
library(stringr)
library(dplyr)
library(leaflet)
library(mapview)
library(tidyverse)
library(here)
library(raster)


manejo_ha_anp <- read_csv("datos_procesados/2019-01-15_manejo_ha_extent_anp.csv")
estilos <- rjson::fromJSON(file = "datos_insumo/spatial_layers_styles.json")
estilos_df <- estilos %>% 
    flatten() %>% 
    map_df(~tibble(layer = .$layer, labels = list(.$data_labels), 
        colors = list(.$colors)))

source("reportes/scripts/funciones_reporte.R")

crear_mapas <- function(capa, manejo_ha_anp, estilos_df) {
    mapas_leaflet <- manejo_ha_anp %>% 
        split(.$anp) %>%
        map(~mapa(., capa, estilos_df, mapa_base = TRUE)) 
    walk2(mapas_leaflet, names(mapas_leaflet), ~mapshot(.x, 
        file = paste0(.y, "_", capa, ".png")))
}

crear_mapas("mex_RE_2015_8_clases", manejo_ha_anp, estilos_df)
crear_mapas("mex_LSperdida_2001_2018", manejo_ha_anp, estilos_df)
crear_mapas("mex_ie_2014_250m", manejo_ha_anp, estilos_df)

# para puma solo creamos en ANPs seleccionadas
id07_puma <- read_csv("datos_insumo/calidad_habitat/puma_ponca_anps.csv") %>% 
    filter(!is.na(`Puma concolor`)) %>% 
    pull(ID_07)
manejo_ha_anp_puma <- manejo_ha_anp %>% 
    filter(id_07 %in% id07_puma)
crear_mapas("mex_pconcolorB1suitability_2014", manejo_ha_anp_puma, estilos_df)
# para ponca solo creamos en ANPs seleccionadas
id07_ponca <- read_csv("datos_insumo/calidad_habitat/puma_ponca_anps.csv") %>% 
    filter(!is.na(`Panthera onca`)) %>% 
    pull(ID_07)
manejo_ha_anp_ponca <- manejo_ha_anp %>% 
    filter(id_07 %in% id07_ponca)
crear_mapas("mex_poncaB1suitability_2014", 
    manejo_ha_anp_ponca[manejo_ha_anp_ponca$id_07 == 1302, ], estilos_df)
