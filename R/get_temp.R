get_temp <- function(years, dat, sheet){
  
  temp <- read_excel(dat, sheet, skip = 0, col_names = TRUE)
  temp_C_dat <- data.frame(time = temp$day, temp_C = temp$Slurry_temp_C)
  d.temp_C_dat <- data.frame(time = temp$day, temp_C = temp$d.Slurry_temp_C)
  temp_air_C_dat <- data.frame(time = temp$day, temp_air_C = temp$Air_temp_C)
  
  t_add <- c(rep(seq(0, years * 365 - 365, 365), each = nrow(temp)))
  
  for (i in c("temp_C_dat", "d.temp_C_dat", "temp_air_C_dat")){
    assign(i, do.call("rbind", replicate(years, eval(parse(text = i)) , simplify = FALSE)))
  }
  
  temp_C_dat$time <- temp_C_dat$time + t_add
  d.temp_C_dat$time <-  d.temp_C_dat$time + t_add
  temp_air_C_dat$time <- temp_air_C_dat$time + t_add
  
  return(list(temp_C_dat = temp_C_dat, d.temp_C_dat = d.temp_C_dat, temp_air_C_dat = temp_air_C_dat))
}