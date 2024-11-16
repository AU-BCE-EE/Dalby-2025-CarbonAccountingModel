slurry_app <- function(days, begin, specs, from, slurry, slurry_mass_cum_yr, min_height){ 
  
  years <- floor(days/365)
  
    if(from == 'storage'){
      a <- 's'
    }
    
    if(from == 'digestate'){
      a <- 'd'
    }
  
  mon <- specs[, grepl(paste('^app_t\\d+', a, sep=""), names(specs))]
  apps <- doy(month = mon, begin = begin)$day
  rems <- as.numeric(specs[, grepl(paste('^app\\d+', a, sep=""), names(specs))])
  
  apps <- apps[rems != 0]
  mon <- mon[rems != 0]
  rems <- rems[rems != 0]
  
  t_add <- rep(seq(0, days - 365, 365), each = length(apps))
  appsl <- c(rep(apps, times = years) + t_add, tail(t_add,1) + 365)
  remsl <- rep(rems, length(appsl))/100
  
  min_slurry_mass <- specs[, grepl(paste0('ex_',from,'_area'), names(specs))] * min_height * 1000
  
  
  if(from == 'storage'){
    for (i in appsl){
      if(slurry$slurry_mass[slurry$time == i] > min_slurry_mass){
        if(slurry$slurry_mass[slurry$time == i] > min_slurry_mass + (slurry_mass_cum_yr * remsl[[which(appsl == i)]])){
        slurry$slurry_mass[slurry$time > i] <- 
        slurry$slurry_mass[slurry$time > i] - 
        slurry_mass_cum_yr * remsl[[which(appsl == i)]]
        } else {
        warning('Slurry mass applied is larger than what is present in storage, a lesser amount was removed instead')
        slurry_mass_limited <- slurry$slurry_mass[slurry$time == i] - min_slurry_mass
        slurry$slurry_mass[slurry$time > i] <- slurry$slurry_mass[slurry$time > i] - slurry_mass_limited
        }
      }
    }
    return(slurry_mass_dat = slurry)
  }
  
  if(from == 'digestate'){
    for (i in appsl){
      if(slurry$slurry_mass[slurry$time == i] > min_slurry_mass){
        if(slurry$slurry_mass[slurry$time == i] > min_slurry_mass + (slurry_mass_cum_yr * remsl[[which(appsl == i)]])){
          slurry$slurry_mass[slurry$time > i] <- 
            slurry$slurry_mass[slurry$time > i] - 
            slurry_mass_cum_yr * remsl[[which(appsl == i)]]
        } else {
          warning('Slurry mass applied is larger than what is present in storage, a lesser amount was removed instead')
          slurry_mass_limited <- slurry$slurry_mass[slurry$time == i] - min_slurry_mass
          slurry$slurry_mass[slurry$time > i] <- slurry$slurry_mass[slurry$time > i] - slurry_mass_limited
        }
      }
    }
    return(slurry_digestate_dat = slurry)
  }
}