model <- function(x, y, dat, storage_mode, years, temp_overwrite){
  
  
  tryCatch(
    #try to do this
    {
      out <- abm_farm(dat, storage_mode, years, temp_overwrite, detail_output = F)
    },
    #if an error occurs, tell me the error
    error = function(e) {
      message('An Error Occurred')
      print(e)
    }
  )
  
  if(length(out) == 2) out['call'] <- as.character(out['call'])
  
  input_barn <- read_excel(dat, sheet = 'in-barn', skip = 1, col_names = TRUE)
  input_storage <- read_excel(dat, sheet = 'out-of-barn', skip = 1, col_names = TRUE)
  
  output <- c(list(input_barn = input_barn, input_storage = input_storage), out, dat)
  
  xlsm <- gsub('../inputs/','', dat) 
  xlsx <- gsub('xlsm','xlsx', xlsm)
  
  save_file <- paste0('../outputs/result files/results_', xlsx)
  write.xlsx(output, save_file, append = TRUE, rowNames = TRUE)
  
  return(output)
  
}
