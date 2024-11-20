# Author: Frederik Dalby
# Last modified 09-11-2022
abm_farm <- function(dat, storage_mode = TRUE, years = 3, temp_overwrite = NULL, detail_output = FALSE){

  # check simulation time
  if(years <= 1) stop(paste0('simulation must be more than 1 year to reach steady state, recommended is 3 years and you typed ', years,' years'))
  if(years <=2) warning(paste0('simulations time might be to short to reach steady state, you typed ', years,' years'))
  
  # read demo and determine number of abm runs that has to be done for the barn (due to different sections) 
  s <- read_excel(dat, sheet = "in-barn", skip = 1, col_names = TRUE)
  
  # new xl files
  if('Sections' %in% colnames(s)) {
    sim <- as.numeric(s$Sections[1])
  }
  
  # old xl files
  if(!'Sections' %in% colnames(s)){
    sim <- as.numeric(sum(grepl("Section ", colnames(s))))
  }

  # simulate all sections in the barn
  abm_barn_out <- abm_barn(dat = dat, sim = sim, years = years, temp_overwrite = temp_overwrite)

  # pull out results, both barn emission (used in calcNorm.R) and removed slurry (used in abm_storage.R)
  rem_dat <- abm_barn_out$rem_dat
  barn_dat <- abm_barn_out$barn_dat
  conc_fresh <- abm_barn_out$conc_fresh
  xa_fresh <- abm_barn_out$xa_fresh
  farm_dat <- abm_barn_out$farm_dat

  if (isTRUE(storage_mode)){
  
    # take out temperatures and repeat for years.
    temps <- get_temp(years, dat = dat, sheet = "outside temp")
    
    # simulate storages
    abm_storage_out <- abm_storage(years = years, rem_dat = rem_dat, 
                                   conc_fresh = conc_fresh, xa_fresh = xa_fresh, temps = temps, 
                                   doy = doy, temp_overwrite = temp_overwrite) 
  
  }

  # pull out storage data for calculating emission factors in calcNorm
  storage_dat <- abm_storage_out$storage_dat
  digestate_dat <- abm_storage_out$digestate_dat
  norm <- calcNorm(barn_dat, storage_dat, digestate_dat, ave, days, years, detail_output = detail_output, feed = farm_dat$excreta_dat$feed)
  norm$`Manure excretion` <- data.frame(farm_dat$excreta_dat['manure'], unit = c(rep('g pr batch time', 7), c(rep('gN pr batch time', 2)), rep('kg pr batch time', 6)))
  norm$`Feed composition` <- data.frame(farm_dat$excreta_dat['feed'], unit = c(rep('g pr kg wet feed', 15), c('kg wet feed pr. animal pr. batch')))
  norm$`Feed spillage` <- data.frame(farm_dat$excreta_dat['feed_spill'], unit = c('g pr. animal pr. batch time'))

  return(norm)
}


