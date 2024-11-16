abm_barn <- function(dat, sim, years, temp_overwrite){
  
  # need empty dat frames for holding removed slurry and emission dat from the different sections. 
  rem_dat <- NULL
  barn_dat <- NULL

  # loop through sections
  for (w in 1:sim){
    
    # get all the section and storage specific data
    farm_dat <- get_farm(w, years * 365, dat, temp_overwrite = temp_overwrite)
    barn <- abm(years * 365, wthr_pars = farm_dat$wthr_pars, add_pars = farm_dat$farm_dat)
 
    # add farm_dat needed for storage simulation and more
    barn <- cbind(barn, data.frame(farm_dat$extra_pars), source = 'barn')
    
    # slurry removed to a storage
    rem.rows <- which(barn$slurry_mass_eff > 0 & !duplicated(barn$time))
    slurry_mass_dat <- data.frame(time = c(barn$time[rem.rows]), slurry_mass = c(barn$slurry_mass_eff * barn$f_ex_storage)[rem.rows])
    slurry_digestate <- data.frame(time = c(barn$time[rem.rows]), slurry_mass = c(barn$slurry_mass_eff * barn$f_biogas)[rem.rows])
    xa_fresh <- barn[rem.rows,grepl("m[0-9]_eff_conc|sr[0-9]_eff_conc", names(barn))]
    names(xa_fresh) <- gsub("^xa_|_eff_conc$", "", names(xa_fresh))
    xa_fresh <- as.list(xa_fresh)

    # NTS need to add time to conc_fresh, if variable fresh conc shall work in abm_storage
    conc_fresh <- lapply(names(ABM::man_pars2.0$conc_fresh), function(x) {
                  column_name <- paste0(x, '_eff_conc')
                  out <- barn[rem.rows, column_name]
                  return(out)
    })
    
    names(conc_fresh) <- names(ABM::man_pars2.0$conc_fresh)
    
    conc_fresh$C <- barn[rem.rows, "C_eff_conc"]
    conc_fresh$N <- barn[rem.rows, "N_eff_conc"]
    
    ifelse(barn$use_comp[1] == 'no', conc_fresh[c('xa_dead', 'starch', 'Cfat', 'CP', 'RFd')] <- 0, conc_fresh[c('VSd')] <- 0) 
    
    rem_dat1 <- cbind(slurry_mass_dat, digestate = slurry_digestate, conc_fresh, xa_fresh, farm_dat$extra_pars)
    rem_dat <- rbind(rem_dat, rem_dat1)
    barn_dat <- as.data.frame(rbind(barn_dat, assign(paste("section_type", w, sep = ""), barn)))
    
  }
  
  return(list(barn_dat = barn_dat, rem_dat = rem_dat, conc_fresh = conc_fresh, xa_fresh = xa_fresh, farm_dat = farm_dat))
  
}
