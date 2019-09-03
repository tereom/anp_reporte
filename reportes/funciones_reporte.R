library(docstring)
library(tidyverse)

comma <- function(x){
    #' Returns numbers suitable for printing the value (comma format)
    format(x, digits = 3, big.mark = ",")
}


# Tablas de ecorregión, por ahora cada ANP se asigna a una sola ecorregión, la 
# más prevalente
tabs_anps_eco <- function(mi_anp, mis_anps_eco, mi_eco, manejo_ha_anp,
    anp_nombres, path_rds, 
    salida = "html") {
    #' @param path_rds Path to rds file with area on rings, 
    #'   ex. 2018-08-08_area_rings.RData
    anps_area_rings <- read_rds(path_rds)
    anp_rings_eco <- anps_area_rings %>% 
        distinct() %>% 
        mutate(anp = str_replace(anp, "_ring", "")) %>% 
        rename(ha_anillo = hectareas)
    anp_eco_table <- manejo_ha_anp %>% 
        dplyr::filter(anp %in% mis_anps_eco, !is.na(anp)) %>% 
        left_join(anp_rings_eco, by = c("anp")) %>% 
        mutate(
            ha = comma(round(hectareas)), 
            ha_anillo = comma(round(ha_anillo))
        ) %>% 
        dplyr::select(anp, cat_manejo, ha, ha_anillo) %>% 
        arrange(desc(ha))
    titulo_tab <- c(4)
    names(titulo_tab) <- c(mi_eco)
    anp_eco_table_cl <- anp_eco_table %>% 
        left_join(anp_nombres, by = c("anp" = "anp_sin_acentos")) %>% 
        dplyr::select(anp_corto, cat_manejo, ha, ha_anillo)
    
    if (salida == "html") {
        anp_eco_table_print <- anp_eco_table_cl %>% 
            kable("html", align = c("r", "c", "c", "c"), padding = 10, 
                col.names = c("", "cat", "ANP ha", "Periferia ha")) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                position = "float_right", font_size = 11, full_width = FALSE) %>% 
            row_spec(which(anp_eco_table$anp == mi_anp), bold = F, color = "#79797d", 
                background = "mistyrose") %>% 
            row_spec(1:nrow(anp_eco_table), color = "#79797d")  %>%
            add_header_above(titulo_tab)
    } else {
        anp_eco_table_print <- anp_eco_table_cl %>%
            kable("latex",  align = c("l", "c", "c", "c"), 
                col.names = c("", "cat", "ANP ha", "Periferia ha"))
    }
    return(list(anp_eco_table = anp_eco_table, anp_rings_eco = anp_rings_eco, 
        anp_eco_table_print = anp_eco_table_print))
}

tabs_eco <- function(mi_eco, path_rds, salida = "html") {
    #' @param path_rds Path to rds file with area of each ANP and column , 
    #'   indicating eco-region ex. 2018-08-08_ecorregion.RData
    anp_eco <- read_rds(path_rds)
    anp_eco_table <- anp_eco %>%
        group_by(eco) %>%
        summarise(
            hectareas = round(sum(hectareas)),
            n_anps = n()
        ) %>%
        arrange(hectareas) %>%
        mutate(hectareas = comma(hectareas)) 
    if (salida == "html") {
        anp_eco_table_print <- anp_eco_table %>% 
            kable("html", col.names = c("", "ha", "# ANPs")) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                position = "left", font_size = 12, full_width = FALSE) %>% 
            row_spec(which(anp_eco_table$eco == mi_eco), bold = F, 
                color = "#79797d", background = "mistyrose") 
    } else {
        anp_eco_table_print <- anp_eco_table %>%
            kable("latex",  align = c("l", "c", "c"), 
                col.names = c("", "ha", "# ANPs"))
    }
    return(anp_eco_table_print)
}

# Tablas de región CONANP
tabs_anp_region_conanp <- function(manejo_ha_anp, mi_anp, mis_anps_region, 
    mi_region, anp_rings_eco, anp_nombres, salida = "html") {
    anp_region_table <- manejo_ha_anp %>% 
        dplyr::filter(anp %in% mis_anps_region, region %in% mi_region) %>%
        left_join(anp_rings_eco, by = "anp") %>% 
        left_join(anp_nombres, by = c("anp" = "anp_sin_acentos")) %>% 
        mutate(
            hectareas = comma(round(S_TERRES)), 
            hectareas_ring = comma(round(ha_anillo))
            # anp = str_replace_all(str_sub(anp, start = 1, end = 20), "_", " ")
        ) %>% 
        arrange(desc(hectareas))
    titulo_tab_region <- c(4)
    names(titulo_tab_region) <- mi_region
    anp_region_table_print <- anp_region_table %>% 
        dplyr::select(anp_corto, cat_manejo, hectareas, hectareas_ring) 
    if (salida == "html") {
        anp_region_table_print <- anp_region_table_print %>%
            kable("html", align = c("r", "c", "c", "c"), padding = 10, 
                col.names = c("", "cat", "ANP ha", "Periferia ha")) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                position = "float_right", font_size = 11, full_width = FALSE) %>% 
            row_spec(which(anp_region_table$anp == mi_anp), bold = F, color = "#79797d", 
                background = "mistyrose") %>% 
            row_spec(1:nrow(anp_region_table), color = "#79797d")  %>%
            add_header_above(titulo_tab_region)
    } else {
        anp_region_table_print <- anp_region_table_print %>%
            kable("latex",  align = c("r", "c", "c", "c"),
                col.names = c("", "cat", "ANP ha", "Periferia ha"))
    }
    return(anp_region_table_print)
}

tabs_region_conanp <- function(manejo_ha_anp, mi_region, salida = "html") {
    anp_region_table <- manejo_ha_anp %>% 
        group_by(region) %>%
        summarise(
            hectareas = round(sum(S_TERRES)),
            n_anps = n()
        ) %>%
        arrange(hectareas) %>%
        mutate(hectareas = comma(hectareas)) 
    if (salida == "html") {
        anp_region_table_print <- anp_region_table %>% 
            kable("html", col.names = c("", "ha", "# ANPs")) %>%
            kable_styling(bootstrap_options = c("striped", "hover", 
                "condensed"),
                position = "left", font_size = 11, full_width = FALSE) %>% 
            row_spec(which(anp_region_table$region == mi_region), bold = F, 
                color = "#79797d", background = "mistyrose") 
    } else {
        anp_region_table_print <- anp_region_table %>%
            kable("latex",  align = c("l", "c", "c"),
                col.names = c("", "ha", "# ANPs"))
    }
    return(anp_region_table_print)
}

# Tabla de cobertura MADMEX
tabs_madmex <- function(mi_anp, salida = "html") {
    madmex <- read_csv("../datos_procesados/madmex_hansen/clase_cobertura_2015.csv")
    madmex_table <- madmex %>% 
        dplyr::filter(anp == mi_anp, poligono == "anp") %>% 
        mutate(prop = round(100 * (area_ha / sum(area_ha)), 1)) %>% 
        dplyr::select(clase, prop) %>% 
        arrange(-prop) %>% 
        dplyr::filter(prop > 1)
    
    if (salida == "html") {
        madmex_table_print <- madmex_table %>% 
            kable("html", col.names = c("", "% área")) %>%
            kable_styling(bootstrap_options = c("striped", "hover", 
                "condensed"), position = "float_right", font_size = 11, 
                full_width = FALSE)
    } else {
        madmex_table_print <- madmex_table %>% 
            kable("latex", col.names = c("", "% área")) 
    }
    return(madmex_table_print)
    
}

# función mapas leaflet
mapa <- function(mi_eco_info, capa, estilos_df, mapa_base = FALSE){
    xmin <- mi_eco_info$xmin
    xmax <- mi_eco_info$xmax
    ymin <- mi_eco_info$ymin
    ymax <- mi_eco_info$ymax
    extent <- raster::extent(c(xmin, xmax, ymin, ymax))
    
    estilo <- dplyr::filter(estilos_df, layer == capa)
    
    mapa <- leaflet() %>% 
        fitBounds(lng1 = xmin, lat1 = ymin, lng2 = xmax, lat2 = ymax) %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>% 
        addProviderTiles(providers$Esri.WorldImagery, group = "Terreno") %>% 
        addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/geoportal/wms",
            layers = capa,
            options = WMSTileOptions(format = "image/png", transparent = TRUE), 
            group = "Capa"
        ) %>%
        addLegend("bottomright", colors = estilo$colors[[1]],
            labels = estilo$labels[[1]], 
            opacity = 1, group = "leyenda") %>%
        addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/geoportal/wms",
            layers = "mex_ANPTerr175_2017",
            options = WMSTileOptions(format = "image/png", transparent = TRUE, 
                opacity = 0.5), group = "ANP"
        )
    if (!mapa_base) {
        mapa <- mapa %>% 
            addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/geoportal/wms",
                layers = "mex_ZN_terr_geoITRF08_Mayo18",
                options = WMSTileOptions(format = "image/png", transparent = TRUE, 
                    opacity = 0.6), group = "Núcleo-Pres."
            ) %>% 
            addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/reportes/wms",
                layers = "mex_ANPTerr175_2017_rings",
                options = WMSTileOptions(format = "image/png", transparent = TRUE, 
                    opacity = 0.3), group = "Periferia"
            ) %>% 
            addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/reportes/wms",
                layers = "GapPriorExtrema", group = "GAP",
                options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>%
            addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/reportes/wms",
                layers = "LocPob2500", group = "Loc 2500",
                options = WMSTileOptions(format = "image/png", transparent = TRUE)
            ) %>%
            addLayersControl(
                baseGroups = c("Capa", "Terreno"),
                overlayGroups = c("leyenda", "ANP", "GAP", "Loc 2500", "Periferia", 
                    "Núcleo-Pres."),
                options = layersControlOptions(collapsed = FALSE)
            )  %>%
            hideGroup("GAP") %>% 
            hideGroup("Loc 2500") %>% 
            hideGroup("Periferia") %>% 
            hideGroup("Núcleo-Pres.") %>% 
            leaflet.extras::addFullscreenControl(position = "topleft", 
                pseudoFullscreen = FALSE) %>% 
            leafem::addHomeButton(ext = extent, layer.name = "Centrar ANP", 
                position = "topleft")
    }
    return(mapa)
}

# función mapas png (se crean en preprocesamiento/crear_mapas)
mapa_png <- function(anp, capa) {
    ruta <- paste0("../datos_procesados/mapas_png/", anp, "_", capa, 
        ".png")
    knitr::include_graphics(ruta)    
}

# mapas tth
grafica_tth <- function(mi_anp, mis_anps_eco, mi_anp_corto, path_tth, 
    path_tth_clase, escala_color, escala_alpha, anp_nombres, salida = "html") {
    tth <- read_csv(path_tth)
    tth_clase <- read_csv(path_tth_clase)
    tth_tidy <- tth %>% 
        dplyr::filter((anp %in% mis_anps_eco & poligono == "anp") | anp == mi_anp) %>% 
        mutate(
            clase_etiqueta = case_when(
                poligono == "periferia" ~ "periferia",
                poligono == "zona núcleo" ~ "z.núcleo",
                poligono == "zona preservación" ~ "z.preservación",
                anp == mi_anp ~ mi_anp_corto,
                TRUE ~ "otras")
        ) %>% 
        left_join(anp_nombres, by = c("anp" = "anp_sin_acentos")) %>% 
        dplyr::select(anp, ANP = anp_corto, año = anio, TTH, perdida_ha, 
            clase_etiqueta)
    tth_plot <- ggplot(tth_tidy, aes(label = perdida_ha)) + 
        geom_line(aes(x = año, y = TTH, group = interaction(ANP, clase_etiqueta), 
            color = clase_etiqueta, alpha = clase_etiqueta), 
            show.legend = FALSE) +
        scale_alpha_manual(values = escala_alpha) +
        scale_color_manual(values = escala_color) +
        scale_x_continuous(breaks = 2016:2018) +
        labs(y = "", x = "año", title = "TTH (%)", color = "", 
            alpha = "") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    tth_clase_tidy <- tth_clase %>% 
        dplyr::filter((anp %in% mis_anps_eco & poligono == "anp") | anp == mi_anp) %>% 
        mutate(
            clase_etiqueta = case_when(
                poligono == "periferia" ~ "periferia",
                poligono == "zona núcleo" ~ "z.núcleo",
                poligono == "zona preservación" ~ "z.preservación",
                anp == mi_anp ~ mi_anp_corto,
                TRUE ~ "otras")
        ) %>% 
        left_join(anp_nombres, by = c("anp" = "anp_sin_acentos")) %>% 
        dplyr::select(anp, ANP = anp_corto, año = anio, TTH, perdida_ha, 
            clase_etiqueta, clase)
    tth_clase_plot <- ggplot(tth_clase_tidy, aes(label = perdida_ha)) + 
        geom_line(aes(x = año, y = TTH, group = interaction(ANP, 
            clase_etiqueta), color = clase_etiqueta, alpha = clase_etiqueta)) +
        scale_alpha_manual(values = escala_alpha) +
        scale_color_manual(values = escala_color) +
        scale_x_continuous(breaks = 2016:2018) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = "", x = "año", title = "", color = "",
            alpha = "") +
        facet_wrap(~clase) 
    if (salida == "html") {
        vars_tooltip <- c("ANP", "año", "TTH", "perdida_ha")
        p1 <- ggplotly(tth_plot, tooltip = vars_tooltip, 
            dynamicTicks = FALSE)
        p2 <- ggplotly(tth_clase_plot, tooltip = vars_tooltip, 
            dynamicTicks = FALSE)
        plot_tth <- subplot(style(p1, showlegend = FALSE), p2, margin = 0.03) 
    } else {
        plot_tth <- gridExtra::grid.arrange(tth_plot, tth_clase_plot, nrow = 1, 
            widths = c(0.3, 0.7))
    }
    return(plot_tth)
}

# tabla y gráfica de pérdida región CONANP
tabs_plots_perdida <- function(indice_perdida, mi_anp, mis_anps_region, 
    anp_nombres, salida = "html") {
    # ecorregiones representadas en región CONANP
    mis_eco_region <- indice_perdida %>% 
        dplyr::filter(anp %in% mis_anps_region) %>% pull(eco) %>% unique()
    tab_def_eco <- indice_perdida %>% 
        dplyr::filter(!is.na(eco)) %>% 
        dplyr::select(eco, perdida_eco) %>% 
        distinct() %>% 
        mutate(perdida_eco = round(perdida_eco, 2)) %>% 
        arrange(perdida_eco)
    if (salida == "html") {
        tab_perdida <- tab_def_eco %>% 
            kable("html", col.names = c("", "% perdida")) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                position = "float_right", font_size = 11, full_width = FALSE) %>% 
            row_spec(which(tab_def_eco$eco %in% mis_eco_region), bold = F, 
                color = "#79797d", background = "mistyrose") 
    } else {
        tab_perdida <- tab_def_eco %>% 
            kable("latex", col.names = c("", "% perdida")) 
    }
    
    indice_perdida_tidy <- indice_perdida %>% 
        dplyr::filter(anp %in% mis_anps_region) %>% 
        left_join(anp_nombres, by = c("anp" = "anp_sin_acentos")) %>% 
        arrange(ind_perdida)
    ind_mi_anp <- ifelse(indice_perdida_tidy$anp == mi_anp, "red", "gray30")
    plot_perdida <- ggplot(indice_perdida_tidy,
        aes(x = reorder(anp_corto, ind_perdida), y = ind_perdida, 
            label = eco)) + 
        geom_bar(stat = 'identity', aes(fill = ind_cat), width = .5)  +
        scale_fill_manual(name = "", 
            labels = c("mayor al promedio", "menor al promedio"), 
            values = c("FALSE" = "#f8766d", "TRUE" = "#00ba38")) + 
        labs(title = "Pérdida boscosa en la región", x = "", y = "") + 
        coord_flip() +
        theme(axis.text.y = element_text(colour = ind_mi_anp))
    return(list(tab_perdida = tab_perdida, plot_perdida = plot_perdida))
}

# boxplot integridad y datos para texto
tabs_grafica_ie <- function(mis_anps_eco, mi_anp, mi_anp_corto, path_stats, 
    path_samples, escala_color, anp_nombres) {
    ie_stats <- read_csv(path_stats)
    ie_samples <- read_csv(path_samples)
    mis_ie_samples <- ie_samples %>% 
        dplyr::filter(anp %in% mis_anps_eco, tipo_id == 1)  %>% 
        left_join(anp_nombres, by = c("anp" = "anp_sin_acentos")) %>% 
        mutate(clase = ifelse(anp == mi_anp, mi_anp_corto, "otras")) 
    mis_ie_stats <- ie_stats %>% 
        dplyr::filter(anp %in% mis_anps_eco, tipo_id != 1)  %>% 
        left_join(anp_nombres, by = c("anp" = "anp_sin_acentos")) %>% 
        mutate(
            clase = case_when(
                tipo_id == 2 ~ "periferia",
                tipo_id == 3 ~ "z.núcleo",
                tipo_id == 4 ~ "z.preservación"
            ) 
        )
    mi_anp_ie_stats <- ie_stats %>% 
        dplyr::filter(anp == mi_anp, tipo_id == 1)
    boxplot <- ggplot() +
        coord_flip() +
        geom_boxplot(data = mis_ie_samples, aes(x = reorder(anp_corto, valores, 
            median), y = valores, color = clase), alpha = 0.6, 
            show.legend = FALSE, outlier.color = "gray90", coef = 0) +
        scale_color_manual("", values = escala_color) +
        geom_point(data = mis_ie_stats, aes(x = anp_corto, y = mediana, 
            color = clase), alpha = 0.8) +
        labs(x = ",", title = "Integridad Ecosistémica", y = "")
    
    return(list(boxplot = boxplot, ie_stats = mi_anp_ie_stats))
}


graficas_ih <- function(mi_anp, mis_anps_region, anp_nombres, path_suit, 
    mi_anp_corto, escala_color){
    load(path_suit)
    suit_anp <- tabla_suit %>% 
        dplyr::filter(anp == mi_anp) 
    suit_stats_anp <- suit_stats %>%
        ungroup() %>% 
        dplyr::filter(anp == mi_anp, tipo != "ANP") %>% 
        mutate(
            clase = case_when(
                tipo == "anillo" ~ "periferia",
                tipo == "núcleo" ~ "z.núcleo",
                tipo == "preservación" ~ "z.preservación"
            )
        )
    boxplot_anio <- ggplot(suit_anp, aes(x = factor(anio), y = idoneidad)) +
        geom_boxplot(alpha = 0.6, show.legend = FALSE, 
            outlier.color = "gray90", coef = 0, color = "gray50") +
        geom_point(data = suit_stats_anp, aes(x = factor(anio), y = mediana, 
            color = clase)) +
        scale_color_manual("", values = escala_color) +
        labs(x = "año", y = "", title = "Calidad de hábitat", color = "") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ih_stats_2014 <- suit_stats %>% 
        ungroup() %>% 
        dplyr::filter(anio == 2014, tipo != "ANP", anp %in% mis_anps_region) %>% 
        dplyr::select(anp_sin_acentos = anp, mediana, tipo) %>% 
        left_join(anp_nombres, by = "anp_sin_acentos") %>% 
        mutate(
            clase = case_when(
                tipo == "anillo" ~ "periferia",
                tipo == "núcleo" ~ "z.núcleo",
                tipo == "preservación" ~ "z.preservación"
            )
        ) 
    tabla_suit_region <- tabla_suit %>% 
        rename(anp_sin_acentos = anp) %>%
        dplyr::filter(anio == 2014, anp_sin_acentos %in% mis_anps_region) %>% 
        left_join(anp_nombres,by = "anp_sin_acentos") %>% 
        mutate(
            clase = ifelse(anp_sin_acentos == mi_anp, mi_anp_corto, "otras")
        ) 
    boxplot_region <- ggplot() +
        coord_flip() +
        geom_boxplot(data = tabla_suit_region, aes(x = reorder(anp_corto, 
            idoneidad, median), y = idoneidad, color = clase), alpha = 0.6, 
            show.legend = FALSE, outlier.color = "gray90", coef = 0) +
        geom_point(data = ih_stats_2014, aes(x = anp_corto, 
            y = mediana, color = clase),  alpha = 0.8) +
        scale_color_manual("", values = escala_color) +
        labs(x = ",", title = "Calidad de hábitat", y = "")
    return(list(boxplot_anio = boxplot_anio, boxplot_region = boxplot_region))
}

grafica_ecoregion_ih <- function(mi_anp, mi_anp_corto, mis_anps_eco, anp_nombres, 
    path_suit_all){
    # NO FORMA PARTE DE LA ÚLTIMA VERSIÓN 
    # path_suit_all: loads data.frame for ANP, ring, zp, zn
    # "../datos_procesados/2018-02-12_suitability.RData"
    load(path_suit_all)
    
    tabla_suit_plot <- tabla_suit %>% 
        dplyr::select(anio, anp_sin_acentos = anp, suit = idoneidad) %>% 
        dplyr::filter(anio == 2014) %>% 
        left_join(anp_nombres) %>% 
        mutate(
            clase = ifelse(anp_sin_acentos == mi_anp, mi_anp_corto, "otras")
        ) %>% 
        dplyr::filter(anp_sin_acentos %in% mis_anps_eco) 
    
    tabla_suit_rings_median <- tabla_suit_rings %>% 
        mutate(anp_sin_acentos = anp) %>% 
        dplyr::select(anio, anp_sin_acentos, suit = idoneidad) %>% 
        dplyr::filter(anio == 2014) %>% 
        left_join(anp_nombres) %>% 
        mutate(
            clase = ifelse(anp_sin_acentos == mi_anp, mi_anp_corto, "otras")
        ) %>% 
        dplyr::filter(anp_sin_acentos %in% mis_anps_eco) %>% 
        group_by(anp_corto) %>% 
        summarise(mediana_anillo = median(suit))
    
    ie_boxplot <- ggplot() +
        coord_flip() +
        geom_boxplot(data = tabla_suit_plot, aes(x = reorder(anp_corto, suit, median),
            y = suit, color = clase), alpha = 0.6, show.legend = FALSE, 
            outlier.color = "gray90", coef = 0) +
        scale_color_manual("", values = escala_color) +
        geom_point(data = tabla_suit_rings_median, aes(x = anp_corto, 
            y = mediana_anillo), color = "blue", alpha = 0.8) +
        labs(x = ",", title = "Integridad Ecosistémica", y = "")
    
    ie_boxplot
}


evaluar_snmb <- function(anp, path_textos){
    anps_snmb <- read_csv(path_textos)
    anps_detect <- map_lgl(anps_snmb$anps, 
        ~str_detect(anp, regex(., ignore_case = T)))
    es_snmb <- sum(anps_detect) >= 1
    if (es_snmb) {
        anp_df <- anps_snmb %>% 
            dplyr::filter(anps_detect)
        anp <- pull(anp_df, anps)
    } else{
        anp_df <- NA
        anp <- NA
    }  
    return(list(anp = anp, anp_df = anp_df))
}
