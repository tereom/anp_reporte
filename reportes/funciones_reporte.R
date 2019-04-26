library(docstring)
comma <- function(x){
    #' Returns numbers suitable for printing the value (comma format)
    format(x, digits = 3, big.mark = ",")
}


obtener_anps <- function(mi_anp){
    #' Finds comparable ANPs
    #' 
    #' Given a target ANP the function finds at most 5 other ANPs that belong 
    #' to the same ecorregion and if more tha 5 are found it returns the 5 
    #' of closest size, along with the target ANP.
    #' @param mi_anp The target ANP.
    #' @return A vector with the name of the comparable ANPs.
    mi_info <- manejo_ha_anp %>% 
        filter(anp == mi_anp)
    tab_eco <- filter(manejo_ha_anp, eco == pull(mi_info, eco))
    if (nrow(tab_eco) > 5){
        mi_tamano <-  pull(mi_info, S_TERRES)
        tab_eco <- tab_eco %>% 
            ungroup() %>% 
            mutate(dist = abs(S_TERRES - mi_tamano)) %>% 
            arrange(dist) %>% 
            top_n(n = 6, -dist) 
    }
    pull(tab_eco, anp)
}
