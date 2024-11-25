get_farm <- function(w, days, dat, temp_overwrite = NULL){
  
  COD_conv <- c(CH4 = 0.2507, xa_dead = 0.73, RFd = 0.8444792, iNDF = 0.8444792, starch = 0.8444792, Cfat = 0.3117844, CP = 0.6541602,
                VFA = 0.9383125, non_urea_N = 1/0.9237898, S = 0.5015, VS = 0.69)

  #get information about the different sections 
  sections <- read_excel(path = dat, sheet = "in-barn", skip = 1, col_names = TRUE)
  col_names <- c(t(sections[, (grepl(paste0("Section ", w, "\\b"), colnames(sections)))]))
  sections <- as.data.frame(t(sections[paste0("Value ", w)]))
  colnames(sections) <- gsub("[ ]", "_", col_names)
  sections[!grepl("[a-zA-Z]", sections)] <- as.numeric(sections[!grepl("[a-zA-Z]", sections)]) 

  section_parms <- get_sections(sections)
  attach(section_parms)
  
  # modify parms
  wash_water <-     wash_water * n_anim
  
  # if natural ventilation occurs calculate temperatures from the outside temperature (variable)
  # from DCA report 110, kai the difference between inside and outside air temp is 2.4 deg C in cattle barns.
  # the barn air temperature is used as a proxy of the slurry temperature. 
  if(vent == "natural" & temp_air_C == "variable"){
    
    temp <- read_excel(dat, sheet = "outside temp", skip = 0, col_names = TRUE)
    temp_air_C_dat <- data.frame(time = temp$day, temp_air_C = temp$Air_temp_C + 2.4)
    t_add <- c(rep(seq(0, days - 365, 365), each = nrow(temp)))
    assign("temp_air_C_dat", do.call("rbind", replicate(days/365, eval(parse(text = "temp_air_C_dat")) , simplify = FALSE)))
    temp_air_C_dat$time <- temp_air_C_dat$time + t_add
    temp_air_C <- mean(temp_air_C_dat$temp_air_C) + 2.4
    temp_C_dat <- temp_air_C_dat
    
    if(!is.null(temp_overwrite)) {
      if(temp_overwrite == 'NIR'){
        temp_C_dat$temp_air_C <- ABM::outside_slurry_temp_NIR$temp_C + 2.4
        temp_air_C <- mean(ABM::outside_slurry_temp_NIR$temp_air_C) + 2.4
      }
      if(temp_overwrite == 'Vechi'){
        temp_C_dat$temp_air_C <- ABM::outside_slurry_temp_vechi$temp_C + 2.4
        temp_air_C <- mean(ABM::outside_slurry_temp_vechi$temp_C) + 2.4
      }
    }
    
  }
  
  ### check some arguments ####
  if(biogas == "none" && f_biogas > 0) stop("slurry is exported to biogas, but biogas is selected as \"none\"")
  if(sum_feed < 99 | sum_feed > 101) stop("feed componenets does not add up to 100%")
  if(biogas == "yes" && between(removal_biogas, 40, 80) == FALSE) warning("the removal efficiency in your biogas plant is unusually high or low")
  if(isTRUE(sections < 0)) stop("some section values are negative")

  # import storage settings
  storages <-               read_excel(path = dat, sheet = "out-of-barn", skip = 1, col_names = TRUE)
  storages <-               storages[, !grepl("barn", colnames(storages))]
  col_names <-              c(t(storages[, (grepl(paste0("Storage ", storage_tank_ID, "\\b"), colnames(storages)))]))
  storages <-               as.data.frame(t(storages[,grepl(paste0("Value ", storage_tank_ID, "\\b"), colnames(storages))]))
  colnames(storages) <-     gsub("[ ]", "_", col_names)
  storages[!grepl("[a-zA-Z]", storages)] <- as.numeric(storages[!grepl("[a-zA-Z]", storages)]) 
  
  # check that the storage is active
  if(!is.null(storages$Status)){
    if(storages$Status == 'deactive'){
      stop(paste0('Section ',w,' passes slurry to storage ID ',storage_tank_ID,', but storage ID ', storage_tank_ID,' is deactive. Change to an active storage or activate storage ID ', storage_tank_ID))
    }
  }

  storage_parms <- get_storages(storages)
  attach(storage_parms)
  
  #check error with application pattern. Digestate pattern can be more flexible. 
  if(app_pattern_s != class_anim) stop(paste0('Application pattern: ', app_pattern_s, ', from storage_ID ', storage_ID, ' does not match anim_class: ', class_anim))

  # get information about composition of excreted manure
  if (class_anim == "pig" & type_anim == "finisher" | type_anim == "piglet") type <- "Grow"
  if (class_anim == "pig" & type_anim != "finisher" & type_anim != "piglet") type <- "Adult"
  if (class_anim == "cattle") type <- ""

  feed_dat <- read_excel(paste0('../feed tables/', class_anim,'_table.xlsx', sep = ""), 
                         skip = 0, col_names = TRUE) 
  names <- c("Feed_name", "DM", 'NSP', 'sNSP', "CP", "Ex_Cfat", paste("Ex_CP", type, sep = ""), "Ex_Starch", paste("Ex_deg_ResFib", type, sep = ""), paste("Ex_nondeg_ResFib", type, sep = ""), 
    paste("Ex_OM", type, sep = ''), 'OM', 'N', 'Cfat', 'NDF', 'Sugar', 'Starch', 'iNDF', 'UndigStarch', 'ResFib', 'Potassium', 'Sodium')
  
  feed_dat <- feed_dat[which(feed_dat$Feed_name %in% colnames(sections)), which(colnames(feed_dat) %in% names)] %>% distinct(Feed_name, .keep_all = TRUE)
  
  # handle non-numeric in feed dat
  feed_dat_num <- sections[, which(colnames(sections) %in% feed_dat$Feed_name)]
  feed_dat_num[sapply(feed_dat_num, is.character)] <- as.numeric(feed_dat_num[sapply(feed_dat_num, is.character)])
  feed_comp <- data.frame(t(feed_dat_num/100))
  feed_comp <- data.frame(cbind(feed_comp, Feed_name = rownames(feed_comp)))
  feed_dat <- merge(feed_dat, feed_comp[], by = 'Feed_name')
  colnames(feed_dat)[ncol(feed_dat)] <- 'proportion'
  feed_dat <- filter(feed_dat, proportion > 0)
  feed_dat[, -1] <- lapply(feed_dat[, -1], as.numeric)

  # call the function to estimate excreted nutrients, enteric emission etc
  excreta_dat <- feedFun(class_anim, type_anim, type, feed_dat, feed_intake, milk_prod, body_weight, batch_time)
  
  cols <- colnames(feed_dat) %in% c('OM','NDF','CP','Cfat','Starch','Sugar','ResFib','iNDF','NSP','sNSP')

  feed <- colSums(feed_dat[, cols] * feed_dat$DM * feed_dat$proportion/1000)
  
  # add some information for later output
  feed['DM'] <- sum(feed_dat$DM * feed_dat$proportion/1000) * 1000
  feed['ash'] <- feed[['DM']] - feed[['OM']]
  feed['water'] <- 1000 - feed[['DM']]
  
  if(class_anim == 'cattle'){
    feed['sNSP'] <- NA
    feed['NSP'] <- NA
  }
  
  feed['RFd'] <- feed['ResFib'] - feed['iNDF']
  feed['FF'] <- excreta_dat[['fermFib']]
  feed['feed_intake'] <- feed_intake

  feed.names <- c('CP', 'Cfat', 'iNDF', 'RFd', 'Starch', 'Sugar', 'ash', 'water')
  feed_spill <- feed[names(feed) %in% feed.names]/1000  * feed_intake * (feed_spill_frac/100) * 1000 # g of feed components spilled during batch
  
  CH4_ent <- excreta_dat[['CH4_ent']] * 365 / (batch_time + rest_d)
  CO2_ent <- excreta_dat[['CO2_ent']] * 365 / (batch_time + rest_d)
  
  # using feed composition and functions for urine and feces volumes and masses. 
  # NTS water spill is not included here, any solution. Need to infact account for water spillage.
  # Washing water is accounted for elsewhere.
  slurry <- (excreta_dat$manure[['feces']] + excreta_dat$manure[['urine']] + bedding + sum(feed_spill)/1000) + water_spill
  slurry_tot <- slurry * n_anim
  slurry_tot_day <- slurry_tot/batch_time
  feed_DM <- feed['DM']/1000
  solid_manure <- 0

  if(grepl('bedding', floor_type)){
    solid_manure <- slurry_tot * excretion_ratio_deep_litter
    slurry_tot <- slurry_tot * (1 - excretion_ratio_deep_litter)
    slurry_tot_day <- slurry_tot_day * (1 - excretion_ratio_deep_litter)
  }

  slopes.slurry_prod_rate <- NA

  # convert to COD units in slurry (g COD or g element/kg slurry)
  # bedding material adds a little to CP according to normative system values
  
  CP <- (excreta_dat$manure[['CP_feces']] + feed_spill[['CP']] + (bedding * bedding_TS * 0.005 * 6.25)/1000) / COD_conv[['CP']]/slurry
  Cfat <- ((excreta_dat$manure[['Cfat_feces']] + feed_spill[['Cfat']]) / COD_conv[['Cfat']])/slurry
  starch <- ((excreta_dat$manure[['starch_feces']] + feed_spill[['Starch']] + feed_spill[['Sugar']]) / COD_conv[['starch']])/slurry
  RFd <- ((excreta_dat$manure[['RFd_feces']] + feed_spill[['RFd']] - feed_spill[['iNDF']])/ COD_conv[['RFd']])/slurry
  iNDF <- (excreta_dat$manure[['iNDF_feces']] + feed_spill[['iNDF']] + (bedding * bedding_TS - bedding * bedding_TS * 0.005 * 6.25)/1000) / COD_conv[['iNDF']]/slurry
  VFA <- (excreta_dat$manure[['VFA_feces']] / COD_conv[['VFA']])/slurry
  
  # make some additional corrections. Mainly where does the non-urea N in the urine go?. Goes to CP
  # see assumptions below. Ash needs to be corrected. 
  if(class_anim %in% c('pig', 'cattle')){
    urea <- excreta_dat$manure[['urea_N']]/slurry
    CP <- CP + (excreta_dat$manure[['urine_N']] - excreta_dat$manure[['urea_N']])/slurry * 4 * COD_conv[['non_urea_N']] # 4 is g molecule per g N. the molecule is assumed to be average of hippuric acid, allantoin, creatinine = C5.67H7.33N2.67O2.33, with COD of 1.08
    TAN <- 0
    ash <- sum(CP * COD_conv[['CP']], Cfat * COD_conv[['Cfat']], 
               starch * COD_conv[['starch']], RFd * COD_conv[['RFd']],
               iNDF * COD_conv[['iNDF']]) * 0.176 # ash is calculated as 17.6% of the DM in the manure. That is a VS content of 82.4%
  }

  sulfate <- 0.01
  
  # some edits to pH and sulfate conc if we have acidification
  # barn
  if (barn_acid == "none"){
    if (class_anim == "pig") pH <- 7
    if (class_anim == "cattle") pH <- 7
    sulfate_conc <- sulfate
  } else if (barn_acid == "continuous H2SO4 acidification"){
    pH <- H2SO4_titrat(conc_SO4 = barn_acid_dose * 32.06/98.08, class_anim = class_anim)$pH  
    sulfate_conc <- barn_acid_dose * 32.06/98.08
  }
 
  # storage
  if (storage_acid == "none"){
    if (class_anim == "pig") pH_storage <- 7.2
    if (class_anim == "cattle") pH_storage <- 7
    pH_digestate <- 7.66
    sulfate_conc_storage <- sulfate
    sulfate_conc_digestate <- 0
  } 
  if (storage_acid == "continuous H2SO4 acidification"){
    pH_storage <- H2SO4_titrat(conc_SO4 = storage_acid_dose * 32.06/98.08, class_anim = class_anim)$pH
    pH_digestate <- H2SO4_titrat(conc_SO4 = digestate_acid_dose * 32.06/98.08, class_anim = "degassed")$pH
    sulfate_conc_storage <- storage_acid_dose * 32.06/98.08 
    sulfate_conc_digestate <- digestate_acid_dose * 32.06/98.08 
  }
  
  if (storage_acid == "single dose acid"){
    # not implementable yet
  }
  
  # correct if inconsistency in biogas inputs
  if (biogas == 'none'){
    f_storage <- 1
    f_biogas <- 0
  }
  
  # height of slurry
  height <- resid_depth + (slurry_tot_day/1000/area) * as.numeric(empty_int)
  
  # warn about overflowing the storage
  if ((height < resid_depth & !grepl('bedding', floor_type)) | ((height > storage_depth) & !grepl('bedding', floor_type))){
    stop('In get_farm(), slurry height is lower than minimum slurry height or larger than maximum slurry heigh, problem with slurry production versus dimensions og slurry pit - check data input')
  }
  
  average_height <- resid_depth + (height - resid_depth)/2
  
  # The following is a rough estimate of the average slurry temp reduction. Data from Holm et al. 2017 is used. 
  # It is assumed that cooling effect is proportional to reduction in temperature (which is most likely not correct)
  # there is no temperature gradient from the 0.1 m slurry height to the surface.
  if (barn_cool != "none" & average_height <= 0.1){
    cool_temp <- (average_height * 21 - 4.5) * cool_eff/26 
  } else if(barn_cool != "none" & average_height > 0.1){
    cool_temp <- -(2.4 * cool_eff/26 )
  } else {
    cool_temp <- 0
  }
  
  # sort out temperature if it is variable + cooling. 
  # NTS This should not really happen maybe in mechanically ventilated pig barn
  if(exists("temp_air_C_dat")){
    temp_C_dat$temp_C <- temp_air_C_dat$temp_air_C + cool_temp * cool_days/365
    temp_C_dat <- as.data.frame(temp_C_dat)
    temp_air_C_wthr <- mean(temp_air_C_dat$temp_air_C)
    temp_C_wthr <- mean(temp_C_dat$temp_C)
  } else {
    temp_C_dat <- temp_air_C + cool_temp * cool_days/365  
    temp_air_C_dat <- temp_air_C
    temp_air_C_wthr <- temp_air_C_dat
    temp_C_wthr <- temp_C_dat
  }
  
  slurry_prod_rate <- slurry_tot_day
  floor_area <- prod_area
  section_ID <- w

  slopes <- c(urea = NA, slurry_prod_rate = slopes.slurry_prod_rate)
  
  # For the Arrhenius equation 
  VS <- (CP + Cfat + starch + RFd + iNDF) # VS in COD units
  VSd_A <- VS * COD_conv[['VS']] * VSd_VS # convert from COD to mass and correct for degradable in indegradable VS
  VSnd_A <- VS * COD_conv[['VS']] * (1-VSd_VS)
  
  conc_fresh <- list(sulfide = 0, urea = urea, sulfate = sulfate_conc, TAN = TAN, Cfat = Cfat, 
                     CP = CP, starch = starch, RFd = RFd, iNDF = iNDF, VFA = VFA, xa_aer = 0, xa_bac = 0,
                     xa_dead = 0, VSd = 1e-10, VSd_A = VSd_A, VSnd_A = VSnd_A, ash = ash)
    
  lnA <- c('VSd_A' = lnA)
  E_CH4 <- c('VSd_A' = E_CH4)

  # compile farm specific pars for abm simulation, correct radiation and rain because it is inside a building.
  wthr_pars <- list(temp_C = temp_C_wthr, temp_air_C = temp_air_C_wthr, RH = 90, rain = 0, pres_kpa = 101, rs = 10)
  
  farm_dat <- list(slopes = slopes, conc_fresh = conc_fresh, slurry_prod_rate = slurry_prod_rate,
                   storage_depth = storage_depth, resid_depth = resid_depth, rain = 0,
                   empty_int = empty_int, area = area, floor_area = floor_area, 
                   wash_water = wash_water, wash_int = wash_int, rest_d = rest_d, 
                   temp_air_C = temp_air_C_dat, pH = pH, temp_C = temp_C_dat,
                   graze = graze, lnA = lnA, VS_CH4 = VS_CH4, E_CH4 = E_CH4, 
                   kl.NH3 = kl_NH3_pit, kl.NH3_floor = kl_NH3_floor)
 
  extra_pars <- list(type_anim = type_anim, n_anim = n_anim, class_anim = class_anim, batch_time = batch_time, 
                       prod_area = prod_area, barn_acid = barn_acid, barn_acid_dose = barn_acid_dose, 
                       solid_liquid = solid_liquid, removal_SLS = removal_SLS, water_spill = water_spill,
                       wash_water = wash_water, f_ex_storage = f_ex_storage, f_biogas = f_biogas, removal_biogas = removal_biogas, 
                       section_ID = section_ID, rest_d = rest_d, ex_storage_rain = ex_storage_rain, ex_storage_radiation = ex_storage_radiation,
                     ex_storage_area = ex_storage_area, ex_storage_depth = ex_storage_depth,  
                       storage_ID = storage_ID, storage_acid = storage_acid, storage_acid_dose = storage_acid_dose,
                       cover_s = cover_s, venting_s = venting_s, venting_eff_s = venting_eff_s, 
                       flaring_s = flaring_s, flaring_eff_s = flaring_eff_s,  
                       pH_storage = pH_storage, sulfate_conc_storage = sulfate_conc_storage,
                     digestate_area = digestate_area, digestate_depth = digestate_depth, 
                       digestate_ID = digestate_ID, digestate_acid = digestate_acid, digestate_acid_dose = digestate_acid_dose,
                       cover_d = cover_d, venting_d = venting_d, venting_eff_d = venting_eff_d, 
                       flaring_d = flaring_d, flaring_eff_d = flaring_eff_d,
                     pH_digestate = pH_digestate, sulfate_conc_digestate = sulfate_conc_digestate,
                     app1s = app1s, app2s = app2s, app3s = app3s, app4s = app4s, app5s = app5s, app6s = app6s, app7s = app7s, app8s = app8s, app9s = app9s, app10s = app10s, app11s = app11s, app12s = app12s,
                     app_t1s = app_t1s, app_t2s = app_t2s, app_t3s = app_t3s, app_t4s = app_t4s, app_t5s = app_t5s, app_t6s = app_t6s, app_t7s = app_t7s, app_t8s = app_t8s, app_t9s = app_t9s, app_t10s = app_t10s, app_t11s = app_t11s, app_t12s = app_t12s,
                     app1d = app1d, app2d = app2d, app3d = app3d, app4d = app4d, app5d = app5d, app6d = app6d, app7d = app7d, app8d = app8d, app9d = app9d, app10d = app10d, app11d = app11d, app12d = app12d,
                     app_t1d = app_t1d, app_t2d = app_t2d, app_t3d = app_t3d, app_t4d = app_t4d, app_t5d = app_t5d, app_t6d = app_t6d, app_t7d = app_t7d, app_t8d = app_t8d, app_t9d = app_t9d, app_t10d = app_t10d, app_t11d = app_t11d, app_t12d = app_t12d,
                     E_CH4 = E_CH4, VS_CH4 = VS_CH4, lnA = lnA, kl_NH3_storage = kl_NH3_storage, 
                     CH4_ent = CH4_ent, CO2_ent = CO2_ent, MCF_CH4_solid_manure = MCF_CH4_solid_manure, solid_manure = solid_manure, excretion_ratio_deep_litter = excretion_ratio_deep_litter
                     )

  excreta_dat$feed_spill <- feed_spill
  excreta_dat$manure['bedding'] <- bedding
  excreta_dat$manure['feed_spill'] <- sum(feed_spill)/1000
  excreta_dat$manure['water_spill'] <- water_spill
  excreta_dat$manure['slurry'] <- slurry
  
  # correct to wet feed intake
  feed['feed_intake'] <- ifelse(class_anim == 'cattle', feed['feed_intake']/(feed['DM']/1000), feed['feed_intake'])
  excreta_dat$feed <- feed

  detach(section_parms)
  detach(storage_parms)
  
  return(c(list(farm_dat = farm_dat, extra_pars = extra_pars, wthr_pars = wthr_pars, excreta_dat = excreta_dat)))

}

