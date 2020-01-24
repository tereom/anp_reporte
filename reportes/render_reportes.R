
anp_ids <- readr::read_csv("datos_procesados/nombres_anps_id.csv")

render_reporte <- function(anp, dir_save = "reportes_html", 
    rmd_file = "reportes/prototipo.Rmd", ext = ".html") {
    print(anp)
    id_07 <- anp_ids$id_07[anp_ids$anp == anp]
    rmarkdown::render(rmd_file, 
        output_file = stringr::str_c(id_07, ext), 
        output_dir = stringr::str_c("reportes/", dir_save),
        params = list(mi_anp = anp)
    )
}
safely_render <- purrr::safely(render_reporte)

render_reporte(anp = "anp_terrestres_2017_NOMBRE_Calakmul")


# HTMLs
# quitamos ANPs que pertenecen a más de una región
quitar <- c("anp_terrestres_2017_NOMBRE_C.A.D.N.R._043_Estado_de_Nayarit", 
    "anp_terrestres_2017_NOMBRE_Islas_del_Golfo_de_California")

# truena la isa isabel (58), rio bravo del norte, no tiene superficie?
anp_nombres_cl <- dplyr::filter(anp_ids, !(anp %in% quitar))
purrr::walk(anp_nombres_cl$anp, safely_render, 
    dir_save = "2019-08-20_reportes_id07_html")

purrr::walk(anp_nombres_cl$anp, safely_render, 
    rmd_file = "reportes/prototipo_pdf.Rmd",
    dir_save = "2019-08-20_reportes_id07_pdf", ext = ".pdf")


# revisión de creados
reportes_generados <- list.files("reportes/2019-08-20_reportes_id07_html/", 
    pattern = ".html") %>% 
    tools::file_path_sans_ext()

reportes_generados_pdf <- list.files("reportes/2019-08-20_reportes_id07_pdf/", 
    pattern = ".pdf") %>% 
    tools::file_path_sans_ext()

anps_faltantes <- anp_nombres_cl %>% 
    dplyr::filter(!(id_07 %in% reportes_generados_pdf)) %>% 
    dplyr::pull(anp)

purrr::walk(anps_faltantes, safely_render,
    dir_save = "2019-08-20_reportes_id07_html")

# PDFs
map(anp_nombres_cl$anp_sin_acentos, ~render_reporte(.,  
    dir_save = "2019-08-30_reportes_pdf", 
    rmd_file = "reportes/prototipo_pdf_v2.Rmd"))



