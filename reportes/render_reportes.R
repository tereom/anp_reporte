library(tidyverse)
library(stringr)
library(knitr)
library(rmarkdown)

anp_nombres <- read_delim("../datos_insumo/anp_nombres.tsv", "\t", 
    escape_double = FALSE, trim_ws = TRUE)

render_reporte <- function(mi_anp) {
    try(rmarkdown::render("../reportes/prototipo_tabs.Rmd", 
        output_file = str_c(mi_anp, ".html"), 
        output_dir = "2017-10-24_reportes_html",
        params = list(mi_anp = mi_anp)
        ))
    mi_anp
}

# quitamos ANPs que pertenecen a más de una región
quitar <- c("anp_terrestres_2017_NOMBRE_C.A.D.N.R._043_Estado_de_Nayarit", 
    "anp_terrestres_2017_NOMBRE_Islas_del_Golfo_de_California")

anp_nombres_cl <- filter(anp_nombres, !(anp_sin_acentos %in% quitar))

reportes <- map(anp_nombres_cl$anp, render_reporte)
