library(tidyverse)
library(fs)

nombres_anp <- read_csv("datos_procesados/nombres_anps_id.csv")

# ---------------- escribir tabla ecorregión ------------
load(file = "datos_procesados/area_ecorregion/2018-08-08_ecorregion.RData")
ecoregion_anp_df <- anp_eco_df %>% 
    ungroup() %>% 
    mutate(nombre = NOMBRE, hectareas = round(hectareas, 3)) %>% 
    select(id_07, anp, nombre = NOMBRE, ecorregion = eco, hectareas) %>% 
    na.omit() %>% 
    arrange(id_07)

write_csv(ecoregion_anp_df, path = "datos_procesados/descarga/ecoregion_area.csv")

paths_area <- dir_ls("datos_procesados/area_ecorregion/", regexp = "_area")
    
anps_area <- map_df(set_names(paths_area, basename(paths_area)), read_rds, 
    .id = "file") %>% 
    mutate(anp = str_remove(anp, "_ring")) %>% 
    left_join(nombres_anp, by = "anp") %>% 
    mutate(            
        poligono = case_when(
            str_detect(file, "sinBuffer") ~ "anp_area_ha",
            str_detect(file, "rings") ~ "periferia_area_ha", 
            str_detect(file, "zonasnucleo") ~ "zona_nucleo_area_ha",
            str_detect(file, "zonaspreservacion") ~ "zona_preservacion_area_ha"
            )
        ) %>% 
    select(-file) %>% 
    spread(poligono, hectareas)
write_csv(anps_area, path = "datos_procesados/descarga/area_anp_periferia_zn_zp.csv")

### Cobertura, pérdida
paths_madmex_hansen <- dir_ls("datos_procesados/madmex_hansen", glob = "*.RData")
crea_tab_cobertura <- function(tipo){
    paths <- dir_ls("datos_procesados/madmex_hansen/", 
        regexp = paste0(tipo, "[.]RData"))
    tab <- map_df(set_names(paths, basename(paths)), read_rds, .id = "file") %>% 
        mutate(
            poligono = case_when(
                str_detect(file, "sinBuffer") ~ "anp",
                str_detect(file, "rings") ~ "periferia", 
                str_detect(file, "zonasnucleo") ~ "zona núcleo",
                str_detect(file, "zonaspreservacion") ~ "zona preservación"
                ),
            area_ha = n * 9 / 100
            ) %>% 
        select(-file, -n) %>% 
        left_join(nombres_anp, by = "anp") %>% 
        select(id_07, anp, nombre, poligono, everything())
    tab
}
perdida_hansen <- crea_tab_cobertura("hansen_loss") %>% 
    rename(anio = year_loss, perdida_ha = area_ha)
write_csv(filter(perdida_hansen,anio != 2000), 
    path = "datos_procesados/descarga/perdida_cobertura_2001_2018.csv")
write_csv(perdida_hansen, 
    path = "datos_procesados/madmex_hansen/perdida_cobertura_2001_2018.csv")

etiquetas <- read_csv("datos_insumo/leyenda_madmex_8.csv")
cobrtura_madmex <- crea_tab_cobertura("madmex") %>% 
    left_join(etiquetas, by = "clase_madmex") %>% 
    rename(clase = etiqueta) %>% 
    select(-clase_madmex) 
write_csv(cobrtura_madmex, path = "datos_procesados/descarga/clase_cobertura_2015.csv")
write_csv(cobrtura_madmex, path = "datos_procesados/madmex_hansen/clase_cobertura_2015.csv")

crea_tab_cobertura("madmex_loss") %>% 
    left_join(etiquetas, by = "clase_madmex") %>% 
    rename(anio = year_loss, perdida_ha = area_ha, clase = etiqueta) %>% 
    filter(anio != 2000) %>% 
    write_csv(path = "datos_procesados/descarga/perdida_cobertura_clase_2001_2018.csv")

crea_tab_cobertura("madmex_loss") %>% 
    left_join(etiquetas, by = "clase_madmex") %>% 
    rename(anio = year_loss, perdida_ha = area_ha, clase = etiqueta) %>% 
    write_csv(path = "datos_procesados/madmex_hansen/perdida_cobertura_clase_2001_2018.csv")


#### Integridad

integridad <- read_csv("datos_procesados/integridad/2018-07-27_ie_stats.csv") %>% 
    mutate(
        poligono = case_when(
            tipo_id == 1 ~ "anp",
            tipo_id == 2 ~ "periferia", 
            tipo_id == 3 ~ "zona núcleo", 
            tipo_id == 4 ~ "zona preservación"
            )
        ) %>% 
    select(id_07, anp, nombre, poligono, media:desv_est) %>% 
    na.omit() %>% 
    arrange(id_07)
write_csv(integridad, path = "datos_procesados/descarga/integridad_estadisticas.csv")


### Idoneidad de hábitat
# en script llenar_db.R