library(tidyverse)
library(stringr)
library(knitr)
library(rmarkdown)

anp_nombres <- read_delim("datos_insumo/anp_nombres.tsv", "\t", 
    escape_double = FALSE, trim_ws = TRUE)

render_reporte <- function(mi_anp, dir_save = "reportes_html", rmd_file = "reportes/prototipo.Rmd") {
    try(rmarkdown::render(rmd_file, 
        output_format = "all",
        output_file = str_c(mi_anp, ".pdf"), 
        output_dir = str_c("reportes/", dir_save),
        params = list(mi_anp = mi_anp)
        ))
    mi_anp
}

# HTMLs
# quitamos ANPs que pertenecen a más de una región
quitar <- c("anp_terrestres_2017_NOMBRE_C.A.D.N.R._043_Estado_de_Nayarit", 
    "anp_terrestres_2017_NOMBRE_Islas_del_Golfo_de_California")

# truena la isa isabel (58), rio bravo del norte no tiene superficie?
anp_nombres_cl <- filter(anp_nombres, !(anp_sin_acentos %in% quitar))
reportes <- map(anp_nombres_cl$anp_sin_acentos, render_reporte)

# revisión de creados
reportes_generados <- list.files("reportes/2017-10-24_reportes_html/") %>% 
    tools::file_path_sans_ext()

anps_faltantes <- anp_nombres_cl %>% 
    filter(!(anp_sin_acentos %in% reportes_generados)) %>% 
    pull(anp_sin_acentos)

reportes <- map(anps_faltantes, render_reporte)

# PDFs
reportes <- map(anp_nombres_cl$anp_sin_acentos, ~render_reporte(.,  
    dir_save = "2017-11-10_reportes_pdf", rmd_file = "reportes/prototipo_pdf.Rmd"))

    