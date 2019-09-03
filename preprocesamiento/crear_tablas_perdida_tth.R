library(tidyverse)
perdida <- read_csv("datos_procesados/madmex_hansen/perdida_cobertura_2001_2018.csv")
madmex <- read_csv("datos_procesados/madmex_hansen/clase_cobertura_2015.csv")


# cálculo de pérdida como porcentaje del área total y en hectáreas
perdida_anual_porcentaje <- perdida %>% 
    group_by(anp, poligono) %>% 
    mutate(
        area_total = sum(perdida_ha),
        perdida_prop = round(perdida_ha / area_total * 100, 3)
    ) %>% 
    ungroup() %>% 
    complete(anio = 2000:2018, nesting(id_07, anp, nombre, poligono), 
        fill = list(perdida_ha = 0, perdida_prop = 0))

write_csv(filter(perdida_anual_porcentaje, anio > 2000, !is.na(area_total)), "perdida_anp_anual.csv")

# cálculo de pérdida por clase de vegetación (bosque y selva solamente)
perdida_clase <- read_csv("datos_procesados/madmex_hansen/perdida_cobertura_clase_2001_2018.csv")

perdida_anual_cobertura_porcentaje <- perdida_clase %>% 
    group_by(anp, poligono, clase_madmex) %>% 
    mutate(
        area_total = sum(perdida_ha),
        perdida_prop = round(perdida_ha / area_total * 100, 3)
    ) %>%  
    ungroup() %>% 
    complete(anio = 2000:2018, nesting(clase, clase_madmex), 
        nesting(id_07, anp, nombre, poligono), 
        fill = list(perdida_ha = 0, perdida_prop = 0)) %>% 
    filter(anio > 2000, clase_madmex %in% c(1:2)) %>% 
    mutate(clase_madmex = case_when(
        clase_madmex == 1 ~ "bosque", 
        clase_madmex == 2 ~ "selva"
    ))

write_csv(filter(perdida_anual_cobertura_porcentaje, anio > 2000, 
    !is.na(area_total)), "perdida_anp_anual_clase.csv")

## tth requiere cobertura a tiempo 0, usamos 2015
madmex_boscoso <- madmex %>% 
    filter(clase %in% c("bosque templado", "selva")) %>% 
    group_by(id_07, anp, nombre, poligono) %>% 
    summarise(area_ha = sum(area_ha)) %>% 
    ungroup()

perdida_clase_anps_df_complete <- perdida_clase %>% 
    complete(anio = 2000:2018, nesting(clase, clase_madmex), 
        nesting(id_07, anp, nombre, poligono), 
        fill = list(perdida_ha = 0))

tasa_cambio_aux <- perdida_clase_anps_df_complete %>%
    filter(clase_madmex %in% 1:2, anio >= 2015) %>% 
    group_by(anio, anp, poligono) %>% 
    summarise(perdida_ha = sum(perdida_ha)) %>% 
    left_join(madmex_boscoso, by = c("anp", "poligono")) %>%
    ungroup() %>% 
    arrange(anp, poligono)

tasa_cambio <- tasa_cambio_aux %>% 
    group_by(anp, poligono) %>% 
    mutate(
        cum_perdida = cumsum(perdida_ha),
        area_restante = area_ha - cum_perdida, 
        tasa_cambio = round(100 * (area_restante / lag(area_restante) - 1), 3)
    ) %>% 
    arrange(anp, poligono) %>% 
    filter(anio > 2015) %>% 
    ungroup() %>% 
    select(anp, anio, tasa_cambio, poligono)

tasa_cambio_cl <- tasa_cambio %>% 
    left_join(perdida_anual_porcentaje, by = c("anp", "anio", "poligono")) %>% 
    select(id_07, anp, nombre, poligono, anio, TTH = tasa_cambio, 
        area_bosque_selva = area_total, perdida_ha)
write_csv(filter(tasa_cambio_cl, !is.na(area_bosque_selva)), 
    "datos_procesados/madmex_hansen/tth_anual.csv")
# TTH por clase
tasa_cambio_clase_aux <- perdida_clase_anps_df_complete %>%
    filter(clase_madmex %in% 1:2, anio >= 2015) %>% 
    left_join(madmex, by = c("id_07", "anp", "nombre", "poligono", "clase")) %>%
    arrange(anp, poligono) %>% 
    na.omit()

tasa_cambio_clase <- tasa_cambio_clase_aux %>% 
    group_by(anp, poligono, clase) %>% 
    mutate(
        cum_perdida = cumsum(perdida_ha),
        area_restante = area_ha - cum_perdida, 
        tasa_cambio = round(100 * (area_restante / lag(area_restante) - 1), 3)
    ) %>% 
    arrange(anp, poligono, clase) %>% 
    filter(anio > 2015) %>% 
    ungroup() %>% 
    select(anp, anio, poligono, clase, tasa_cambio)

tasa_cambio_clase_cl <- tasa_cambio_clase %>% 
    left_join(perdida_anual_cobertura_porcentaje, 
        by = c("anp", "anio", "poligono", "clase")) %>% 
    select(id_07, anp, nombre, poligono, clase, anio, TTH = tasa_cambio, 
        area_total, perdida_ha)
write_csv(filter(tasa_cambio_clase_cl, !is.na(area_total)), 
    "datos_procesados/madmex_hansen/tth_anual_clase.csv")
