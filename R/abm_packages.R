abm_packages <- function(){
  x<-c('ABM', 'readxl', 'dplyr', 'openxlsx')
  lapply(x, require, character.only = TRUE)
}

