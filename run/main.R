# Runs all scenarios for sensitivity predicitons
rm(list = ls())

library(tidyr)
library(ggplot2)
library(ABM)

# Load R functions
ff <- list.files('../R', full.names = TRUE) 
for (i in ff) source(i)

# Get input file names
fs <- list.files("../inputs/", full.names = TRUE)
fs <- fs[grepl('xlsm', fs)]

output <- NULL

for (i in fs){

  years <- 4

  dat <- i
  
  Catch_errors <- function(x, y){
    tryCatch(
      #try to do this
      {
       out <- abm_farm(dat, storage_mode = TRUE, years, temp_overwrite = 'Vechi')
      },
      #if an error occurs, tell me the error
      error = function(e) {
        message('An Error Occurred')
        print(e)
      }
    )
  }
    
 out <- Catch_errors(x,y)

 # temporary work around a bug. 
 # That is, if multiple sections are simulated 
 # The out does not contain excreta data.
 
 if(is.null(out$norm)){
   out <- out
 } else{
   out <- out$norm
 }
  
  if(length(out) == 2) out['call'] <- as.character(out['call'])

  input_barn <- read_excel(i, sheet = 'in-barn', skip = 1, col_names = TRUE)
  input_storage <- read_excel(i, sheet = 'out-of-barn', skip = 1, col_names = TRUE)
  
  output1 <- c(list(input_barn = input_barn, input_storage = input_storage), out, i)
  
  # combine outputs
  output <- c(output, output1)

  xlsm <- gsub('../inputs/','', i) 
  xlsx <- gsub('xlsm','xlsx', xlsm)
  
  save_file <- paste0('../outputs/', xlsx)
  
  write.xlsx(output1, save_file, append = TRUE, rowNames = TRUE)

}

