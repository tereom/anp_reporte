library(readr)
library(dplyr)
library(stringr)
library(knitr)
library(rmarkdown)

anp_nombres <- readr::read_csv("datos_procesados/nombres_anps_id.csv")

render_reporte <- function(mi_anp, dir_save = "reportes_html", 
    rmd_file = "reportes/prototipo.Rmd", ext = ".html") {
    id_07 <- anp_nombres$id_07[anp_nombres$anp == mi_anp]
    try(rmarkdown::render(rmd_file, 
        output_file = str_c(id_07, ext), 
        output_dir = str_c("reportes/", dir_save),
        params = list(mi_anp = mi_anp)
    ))
    mi_anp
}

render_reporte("anp_terrestres_2017_NOMBRE_Calakmul")

# HTMLs
# quitamos ANPs que pertenecen a más de una región
quitar <- c("anp_terrestres_2017_NOMBRE_C.A.D.N.R._043_Estado_de_Nayarit", 
    "anp_terrestres_2017_NOMBRE_Islas_del_Golfo_de_California")

# truena la isa isabel (58), rio bravo del norte no tiene superficie?
anp_nombres_cl <- dplyr::filter(anp_nombres, !(anp %in% quitar))
reportes <- purrr::map(anp_nombres_cl$anp, render_reporte, 
    dir_save = "2019-05-13_reportes_id07_html")

# revisión de creados
reportes_generados <- list.files("reportes/2019-05-13_reportes_id07_html/", 
    pattern = ".html") %>% 
    tools::file_path_sans_ext()

anps_faltantes <- anp_nombres_cl %>% 
    filter(!(anp_sin_acentos %in% reportes_generados)) %>% 
    pull(anp_sin_acentos)

reportes <- purrr::map(anps_faltantes, render_reporte,
    dir_save = "2019-05-13_reportes_html")

# PDFs
reportes <- map(anp_nombres_cl$anp_sin_acentos, ~render_reporte(.,  
    dir_save = "2017-11-10_reportes_pdf", rmd_file = "reportes/prototipo_pdf.Rmd"))


# PDFs faltantes
reportes_generados <- list.files("reportes/2017-11-10_reportes_pdf/", 
        pattern = ".pdf") %>% 
    tools::file_path_sans_ext()
    
anps_faltantes <- anp_nombres_cl %>% 
    filter(!(anp_sin_acentos %in% reportes_generados)) %>% 
    pull(anp_sin_acentos)

reportes <- map(anps_faltantes, ~render_reporte(.,  
    dir_save = "2017-11-10_reportes_pdf", 
    rmd_file = "reportes/prototipo_pdf.Rmd"), 
    ext = ".pdf")
    
anps_interes <- c("anp_terrestres_2017_NOMBRE_Janos", 
    "anp_terrestres_2017_NOMBRE_Calakmul", 
    "anp_terrestres_2017_NOMBRE_Montes_Azules", 
    "anp_terrestres_2017_NOMBRE_Tehuacan-Cuicatlan")

reportes <- purrr::map(anps_interes, ~render_reporte(.,  
    rmd_file = "prototipo.Rmd", dir_save = "2018-02-22_reportes_ej"))
