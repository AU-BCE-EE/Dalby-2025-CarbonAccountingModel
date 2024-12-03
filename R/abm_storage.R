abm_storage <- function(years, rem_dat, conc_fresh, xa_fresh, temps, doy = doy, temp_overwrite = NULL){
  
  #data frames for holding results. 
  stor_dat <- NULL
  storage_dat <- NULL
  digestate_dat <- NULL
  
  if(exists('slurry_mass')) {rm('slurry_mass')}

  # repeat this loop for number of storages simulated
  for (i in 1:length(unique(rem_dat$storage_ID))){
    
    if(any(rem_dat$slurry_mass > 0)){
      
      # take out slurry mass to the first storage and sort with respect to time
      stor_dat <- rem_dat %>% filter(storage_ID == i) %>% arrange(time) 
      specs <- stor_dat[1,]
      
      if(specs$class_anim == 'pig'){
        
        arrh_pars <- ABM::arrh_pars_pig2.0
        grp_pars <- ABM::grp_pars_pig2.0
        
      } else if(specs$class_anim == 'cattle'){
        
        arrh_pars <- ABM::arrh_pars_cattle2.0
        grp_pars <- ABM::grp_pars_cattle2.0
        
      } else{
        
        arrh_pars <- ABM::arrh_pars2.0
        grp_pars <- ABM::grp_pars2.0
        
      }
      
      # calculated slurry weighted average from different sections of conc_fresh and xa_fresh for input to storage 
      inflows <- as.data.frame(stor_dat) %>% select(names(conc_fresh), names(xa_fresh), slurry_mass, section_ID) %>% 
        group_by(section_ID) %>%
        summarise(across(c(names(conc_fresh), names(xa_fresh)), mean), across(slurry_mass, sum)) %>%
        mutate(across(-c(slurry_mass, section_ID), function(x) x * slurry_mass/sum(slurry_mass))) %>% 
        summarise_all(sum) 

      storage_conc_fresh <- as.list(inflows[names(conc_fresh)])
      storage_xa_fresh <- setNames(as.numeric(inflows[names(xa_fresh)]), names(xa_fresh))

      # sort out slurry_mass data.frame for storage
      slurry_mass_dat <- rbind(c(0,0), stor_dat[, c("time", "slurry_mass")] %>% mutate_at(vars(slurry_mass), cumsum))
      
      # interpolate slurry mass with "constant" method to ensure slurry mass is added in portions, (not continously)
      xout <- sort(c(slurry_mass_dat$time, 1:slurry_mass_dat$time[length(slurry_mass_dat$time)], decreasing = F))
      slurry_mass_dat <- data.frame(time = xout, 
                                          slurry_mass = approx(slurry_mass_dat$time, slurry_mass_dat$slurry_mass, 
                                                               xout = xout, method = "constant")$y)  %>% distinct()
      
      ifelse(specs$cover_s == "tent", rain <- 0, rain <- specs$ex_storage_rain)
      ifelse(specs$cover_s == "tent", rs <- 0, rs <- specs$ex_storage_radiation)
      rain_dat <- rain * slurry_mass_dat$time # assumed constant
      
      temp_dat <- temps$temp_C_dat
      
      if(!is.null(temp_overwrite)) {
        if(temp_overwrite == 'NIR'){
          temp_dat$temp_C <- outside_slurry_temp_NIR$temp_C
          temp_dat$temp_air_C <- outside_slurry_temp_NIR$temp_air_C
        }
        if(temp_overwrite == 'Vechi'){
          temp_dat$temp_C <- outside_slurry_temp_vechi$temp_C
          temp_dat$temp_air_C <- outside_slurry_temp_NIR$temp_air_C
        }
      }
      
      evap_dat <- rep(ABM:::et(cmakk = 0.7, temp_C = mean(temp_dat$temp_C[2:12]), pres_kpa = 101, rs = rs)) * slurry_mass_dat$time
      
      # Subtracting evap and adding rain from the slurry mass (cumulatives) 
      slurry_mass_dat$slurry_mass <- slurry_mass_dat$slurry_mass - evap_dat * specs$ex_storage_area + rain_dat * specs$ex_storage_area
      
      slurry_mass_cum_yr <- max(slurry_mass_dat$slurry_mass[slurry_mass_dat$time > (years-1) * 365]) - min(slurry_mass_dat$slurry_mass[slurry_mass_dat$time > (years-1) * 365])
      
      slurry_mass_dat <- slurry_app(days = years * 365, begin = 'January', 
                                    specs = specs, from = 'storage', slurry = slurry_mass_dat, 
                                    slurry_mass_cum_yr = slurry_mass_cum_yr, min_height = 0.4)
      
      #diff(slurry_mass_dat$slurry_mass)
      
      lengths <- rle(slurry_mass_dat$slurry_mass)
      indices <- cumsum(lengths$lengths)
      keep_indices <- sort(c(1, indices, indices + 1))
      keep_indices <- keep_indices[keep_indices <= nrow(slurry_mass_dat)]
      
      # Filter the data frame
      slurry_mass_dat <- slurry_mass_dat[keep_indices, ]
      
      max_load_stress <- max(slurry_mass_dat$slurry_mass, na.rm = T)/(specs$ex_storage_area * specs$ex_storage_depth * 1000) * 100
      
      if(max_load_stress > 100){
        stop(paste0("slurry mass exceeded storage_ID ",i," capacity with a max load of ",round(max_load_stress,2),"% of its capacity")) 
      } 
      
      if(max_load_stress < 50){
        warning(paste0("slurry loaded to storage_ID ",i," seems low with only ",round(max_load_stress,2),"% of capacity used at max load")) 
      } 
      
      slurry_mass_dat <- slurry_mass_dat[!duplicated(slurry_mass_dat),]
      
      # if storage acidification is applied then correct SO4_conc_fresh passed to storage. 
      if(specs$storage_acid == 'continuous H2SO4 acidification'){
        storage_conc_fresh$sulfate <- specs$sulfate_conc_storage
      }  

      wthr_pars <- list(temp_C = mean(temp_dat$temp_C[2:12]), temp_air_C = mean(temp_dat$temp_C[2:12]), RH = 90, rain = rain, pres_kpa = 101, rs = rs)  
      
      # correct for inputs if solid liquid separation before storage. Not solutes
      if(specs$solid_liquid == 'yes'){
        
        for (j in c('starch', 'xa_aer', 'xa_bac', 'xa_dead','Cfat','RFd','CP','iNDF','VSnd_A','VSd_A','ash', 'VSd')){
          storage_conc_fresh[j] <- storage_conc_fresh[[j]] * (100 - specs$removal_SLS)/100
          
        }
        
        storage_xa_fresh <- storage_xa_fresh * (100 - specs$removal_SLS)/100
        
      }

      # run model for storage
      storage <- cbind(abm(days = years * 365, 1, wthr_pars = wthr_pars, arrh_pars = arrh_pars, grp_pars = grp_pars, 
                           add_pars = list(slurry_mass = slurry_mass_dat, 
                                    conc_fresh = storage_conc_fresh, xa_fresh = storage_xa_fresh, 
                                    pH = specs$pH_storage, cover = specs$cover_s, storage_depth = specs$storage_depth, 
                                    area = specs$ex_storage_area, floor_area = 0, rain = rain, evap = diff(evap_dat)[1],
                                    temp_C = temp_dat, lnA.VSd_A = specs$lnA, E_CH4.VSd_A = specs$E_CH4, VS_CH4 = specs$VS_CH4,
                                    kl.NH3 = specs$kl_NH3_storage)), source = 'storage', storage_ID = specs$storage_ID, 
                                    prod_area = specs$prod_area, n_anim = specs$n_anim, batch_time =  specs$batch_time, wash_water = 0)
      
      storage$temp_air_C <- approx(temp_dat$time, temp_dat$temp_air_C, storage$time)$y
      
      if(specs$venting_s == TRUE | specs$flaring_s == TRUE){
        storage <- mitigate_storage(specs = specs, from = 'storage', storage = storage)
      }
      
      storage_dat <- as.data.frame(rbind(storage_dat, storage))
    }
  }
  
  # if biogas is used then repeat everything as was done for storage. Some exemption see below. 
  if(any(rem_dat$f_biogas != 0)){
  
    for (i in unique(rem_dat$digestate_ID[rem_dat$f_biogas > 0])){  
    
      stor_dat <- rem_dat %>% filter(digestate_ID == i) %>% arrange(time) 
      specs <- stor_dat[1,]
      
      # calculated slurry weighted average from different sections of conc_fresh and xa_fresh for input to storage 
      inflows <- as.data.frame(stor_dat) %>% select(names(conc_fresh), names(xa_fresh), digestate.slurry_mass, section_ID) %>% 
        group_by(section_ID) %>%
        summarise(across(c(names(conc_fresh), names(xa_fresh)), mean), across(digestate.slurry_mass, sum)) %>%
        mutate(across(-c(digestate.slurry_mass, section_ID), function(x) x*digestate.slurry_mass/sum(digestate.slurry_mass))) %>% 
        summarise_all(sum) 
      
      digestate_conc_fresh <- as.list(inflows[names(conc_fresh)])
      
      # Sulfate is completely reduced to H2S in anaerobic digestion
      digestate_conc_fresh$sulfate <- 0
      
      digestate_xa_fresh <- setNames(as.numeric(inflows[names(xa_fresh)]), names(xa_fresh))
      
      # sort out slurry_mass for digestate storage
      slurry_digestate <- rbind(c(0,0), stor_dat[, c("digestate.time", "digestate.slurry_mass")] %>% mutate_at(vars(digestate.slurry_mass), cumsum))
      xout <- sort(c(slurry_digestate$digestate.time, 1:slurry_digestate$digestate.time[length(slurry_digestate$digestate.time)], decreasing = F))
      slurry_digestate_dat <- data.frame(time = xout, slurry_mass = approx(slurry_digestate$digestate.time, slurry_digestate$digestate.slurry_mass, 
                                                                                 xout = xout, method = "constant")$y) %>% distinct()
      # calculate rain and evap data
      ifelse(specs$cover_d == "tent", rain <- 0, rain <- specs$ex_storage_rain)
      ifelse(specs$cover_d == "tent", rs <- 0, rs <- specs$ex_storage_radiation)
      rain_dat <- rain * slurry_digestate_dat$time # assumed constant
      
      temp_dat <- temps$d.temp_C_dat
      
      if(!is.null(temp_overwrite)) {
        if(temp_overwrite == 'NIR'){
          temp_dat$temp_C <- outside_slurry_temp_dig_NIR$temp_C
          temp_dat$temp_air_C <- outside_slurry_temp_dig_NIR$temp_air_C
        }
        if(temp_overwrite == 'Vechi'){
          temp_dat$temp_C <- outside_slurry_temp_dig_vechi$temp_C
          temp_dat$temp_air_C <- outside_slurry_temp_dig_NIR$temp_air_C
        }
      }
      
      
      evap_dat <- rep(ABM:::et(cmakk = 0.7, temp_C = mean(temp_dat$temp_C[2:12]), pres_kpa = 101, rs = rs)) * slurry_digestate_dat$time
      
      # Subtracting evap and adding rain from the slurry mass (cumulatives) 
      slurry_digestate_dat$slurry_mass <- slurry_digestate_dat$slurry_mass - evap_dat * specs$digestate_area + rain_dat * specs$digestate_area
      
      # slurry removal due to soil application
      slurry_digestate_cum_yr <- max(slurry_digestate_dat$slurry_mass[slurry_mass_dat$time > (years-1) * 365]) - min(slurry_digestate_dat$slurry_mass[slurry_digestate_dat$time > (years-1) * 365])
      
      slurry_digestate_dat <- slurry_app(days = years * 365, begin = 'January', 
                                    specs = specs, from = 'digestate', slurry = slurry_digestate_dat, 
                                    slurry_mass_cum_year = slurry_digestate_cum_yr, min_height = 0.4)
      
      lengths <- rle(slurry_digestate_dat$slurry_mass)
      indices <- cumsum(lengths$lengths)
      keep_indices <- sort(c(1, indices, indices + 1))
      keep_indices <- keep_indices[keep_indices <= nrow(slurry_digestate_dat)]
      
      # Filter the data frame to reduce work for ABM
      slurry_digestate_dat <- slurry_digestate_dat[keep_indices, ]
      
      max_load_stress <- max(slurry_digestate_dat$slurry_mass, na.rm = T)/(specs$ex_digestate_area * specs$ex_digestate_depth * 1000) * 100
      
      if(max_load_stress > 100){
        stop(paste0("slurry mass exceeded storage capacity with a max load of ",round(max_load_stress,2),"% of capacity")) 
      } 
      
      if(max_load_stress < 50){
        warning(paste0("slurry loaded to storage seems low with only ",round(max_load_stress,2),"% of capacity used at max load")) 
      } 
      
      # !OBS! Here we subtract slurry organic matter which was consumed in the digester
      # resulting in a lower conc_fresh state variables passed to the digestate storage
      # iNDF is not consumed in digester and therefore the fractoin of degradable OM is calculted first
      C_before <- digestate_conc_fresh$C
      N_before <- digestate_conc_fresh$N
      
      f_COD <- (digestate_conc_fresh[['iNDF']] + digestate_conc_fresh[['xa_dead']] + digestate_conc_fresh[['VFA']] + digestate_conc_fresh[['starch']] + digestate_conc_fresh[['RFd']] + 
                  digestate_conc_fresh[['CP']] + digestate_conc_fresh[['Cfat']] + digestate_conc_fresh[['VSd']])/
        (digestate_conc_fresh[['VFA']] + digestate_conc_fresh[['starch']] + digestate_conc_fresh[['RFd']] + digestate_conc_fresh[['CP']] + 
           digestate_conc_fresh[['Cfat']] + digestate_conc_fresh[['VSd']])
      
      dCOD <- c('VFA', 'xa_dead', 'starch', 'RFd', 'CP', 'Cfat', 'VSd')
      
      digestate_conc_fresh <- as.data.frame(digestate_conc_fresh)
      
      # simulate digestion by reducing OM in state variables according to "stor_dat$removal_biogas"
      digestate_conc_fresh[dCOD] <- digestate_conc_fresh[dCOD] * (1 - f_COD * specs$removal_biogas/100)
      digestate_conc_fresh <- as.list(digestate_conc_fresh)
      
      if(specs$digestate_acid == 'continuous H2SO4 acidification'){
        digestate_conc_fresh$sulfate <- specs$sulfate_conc_digestate
      }  
      
      if(specs$solid_liquid == 'yes') stop('solid liquid separation is not yet available for use with biogas')
      
      wthr_pars <- list(temp_C = mean(temp_dat$temp_C[2:12]), temp_air_C = mean(temp_dat$temp_air_C[2:12]), RH = 90, rain = rain, pres_kpa = 101, rs = 20)  
      
      digestate <- cbind(abm(days = days, 1, wthr_pars = wthr_pars, add_pars = list(slurry_mass = slurry_digestate_dat, conc_fresh = digestate_conc_fresh, xa_fresh = digestate_xa_fresh, 
                                                                              pH = specs$pH_digestate, cover = specs$cover_d, storage_depth = specs$digestate_depth, rain = rain,
                                                                              area = specs$digestate_area, floor_area = 0, temp_C = temp_dat, 
                                                                              lnA.VSd_A = specs$lnA, E_CH4.VSd_A = specs$E_CH4, VS_CH4 = 16.67)), source = 'digestate', 
                                                                              digestate_ID = specs$digestate_ID, prod_area = specs$prod_area, n_anim = specs$n_anim, batch_time = 0, wash_water = 0,
                                                                              C_before = C_before, N_before = N_before)
      
      digestate_dat$temp_air_C <- approx(temp_dat$time, temp_dat$temp_air_C, storage$time)$y
      
      if(specs$venting_s == TRUE | specs$flaring_s == TRUE){
        digestate <- mitigate_storage(specs = specs, from = 'digestate', storage = digestate)
      }
      
      digestate_dat <- as.data.frame(rbind(digestate_dat, digestate))
    }
  }

  return(list(storage_dat = storage_dat, digestate_dat = digestate_dat))
}

