feedFun <- function(class_anim, type_anim, type, feed_dat, feed_intake, milk_prod, body_weight, batch_time){
 
 #everything in this function is pr. animal
 names.feces <- c('CP_feces', 'Cfat_feces', 'RFd_feces', 'iNDF_feces', 'starch_feces')

 if(class_anim == 'pig'){
 
   feed_DM_conc <- sum(feed_dat$DM * feed_dat$proportion)/1000
   DM_intake <- feed_DM_conc * feed_intake
   feed_dat$ash <- 1000 - feed_dat$OM
   feed_dat$CP_feces <- feed_dat$DM * eval(parse(text = paste("feed_dat$Ex_CP", type, sep = ""))) / 1000 * feed_intake * feed_dat$proportion
   feed_dat$Cfat_feces <- feed_dat$DM * as.numeric(feed_dat$Ex_Cfat) / 1000 * feed_intake * feed_dat$proportion
   feed_dat$RFd_feces <- feed_dat$DM * eval(parse(text = paste("feed_dat$Ex_deg_ResFib",type, sep = ""))) / 1000 * feed_intake * feed_dat$proportion
   feed_dat$iNDF_feces <- feed_dat$DM * eval(parse(text = paste("feed_dat$Ex_nondeg_ResFib",type, sep = ""))) / 1000 * feed_intake * feed_dat$proportion
   feed_dat$starch_feces <- feed_dat$DM * feed_dat$Ex_Starch / 1000 * feed_intake * feed_dat$proportion
   feed_dat$OM_feces <- feed_dat$DM * eval(parse(text = paste("feed_dat$Ex_OM",type, sep = ""))) / 1000 * feed_intake * feed_dat$proportion
   feed_dat$OM_intake <- feed_dat$DM * as.numeric(feed_dat$OM) / 1000 * feed_intake * feed_dat$proportion

   manure <- as.data.frame(t(colSums(feed_dat[, names.feces])))
   manure$ash_feces <- sum(manure)/0.88 - sum(manure)
   # subtract VFA from RFd to not count VFA mass twice. 
   
   OM_dig <- (1 - sum(feed_dat$OM_feces)/sum(feed_dat$OM_intake))
   urine <- 2 * DM_intake # kg urine per production period. Source Saman
   fermFib <- sum(feed_dat$DM * feed_dat$ResFib * feed_dat$proportion)/1000 * feed_intake - (manure$iNDF_feces + manure$RFd_feces) # ResFib intake feed - (iNDF in feces + RFd in feces) # iNDF in feces = iNDF in feed = Ex_nondeg_ResFib
   sNSP_intake_d <- sum(feed_dat$sNSP * feed_dat$DM/1000 * feed_dat$proportion) * feed_intake/batch_time # kg/day
   
   if(type == 'Adult') {
     urine <- 2.5 * DM_intake
     
     # H. Jørgensen 2007, fermented fiber, g pr production period
     CH4_ent <- (0.440 + 0.0206 * fermFib/batch_time) * batch_time * 0.708 # g pr production period
     feces <- (5.405 - 6.31 * OM_dig + 0.505 * DM_intake/batch_time) * batch_time 
     urine_N <- (-21.20 + 0.134 * sum(feed_dat$CP * feed_dat$proportion) + 10.15 * (feed_intake/batch_time * feed_DM_conc)) * batch_time # g urine_N per production period
     
   }
   
   if(type_anim == 'piglet'){
     
     urine_N <- 210 # from normsystem report
     feces <- sum(manure[,!grepl('VFA_feces', names(manure))])/0.25/1000 # 25% ± 0.67 (159 individual observations) DM in weaned piglets feces. Source Saman
     CH4_ent <- (-0.61873610 + 0.03215170 * sNSP_intake_d + 0.02521294 * body_weight) * batch_time * 0.708
     
     } else if(type_anim == 'finisher') {
     
     feces <- (5.405 - 6.31 * OM_dig + 0.505 * DM_intake/batch_time) * batch_time 
     urine_N <- (-21.20 + 0.134 * sum(feed_dat$CP * feed_dat$proportion) + 10.15 * (feed_intake/batch_time * feed_DM_conc)) * batch_time # g urine_N per production period
     CH4_ent <- (-0.61873610 + 0.03215170 * sNSP_intake_d + 0.02521294 * body_weight) * batch_time * 0.708
     
     }
   
   urea_N <- urine_N * 0.75 # g urea_N per production period, constant from four papers (add refs)
   manure$VFA_feces <- 7.1 * feces # g VFA / kg fresh feces, n = 63 grower-finisher pigs. 
   manure$RFd_feces <- manure$RFd_feces - manure$VFA_feces * 162/105 # the 162/105 is the amount of g RFd degraded pr g acetate formed according to stoichiometry from Dalby et al. 2024 (resp paper)
   
  #Elvira 2024
  #Fiber_intake_d <- sum(feed_dat$ResFib * feed_dat$DM/1000 * feed_dat$proportion) * feed_intake/batch_time 
  #CH4_ent_TF <- (-0.46 + 0.007 * Fiber_intake_d + 0.03 * body_weight) * batch_time * 0.708
   
  #NSP_intake_d <- sum(feed_dat$NSP * feed_dat$DM/1000 * feed_dat$proportion) * feed_intake/batch_time # g/day
  #CH4_ent_NSP <- (-0.54 + 0.009 * NSP_intake_d + 0.03 * body_weight) * batch_time * 0.708
   
  #sNSP_intake_d <- sum(feed_dat$sNSP * feed_dat$DM/1000 * feed_dat$proportion) * feed_intake/batch_time # kg/day
  #CH4_ent_sNSP <- (-0.62 + 0.03 * sNSP_intake_d + 0.03* body_weight) * batch_time * 0.708
    
  CO2_ent <- 0.136 * body_weight^0.573 * batch_time

  
  
  }
 
 if(class_anim == 'cattle'){
   
   DM_intake <- feed_intake  # kg year, since feed composition is given directly as DM. 
   feed_DM_conc <- 1/sum(feed_dat$proportion/(feed_dat$DM/10) * 100) # kg DM/ kg feed 

   #below is g/year
   feed_dat$ash <- 1000 - feed_dat$OM
   feed_dat.mod <- feed_dat[, ! colnames(feed_dat) %in% c('DM', 'Feed_name', 'proportion')] *  DM_intake * feed_dat$proportion # g pr year
   feed_dat.mod <- data.frame(t(colSums(feed_dat.mod)))
   feed_dat.mod$OM_dig <- feed_dat.mod$OM * 0.73 
   feed_dat.mod$Cfat_dig <- (0.767 * feed_dat.mod$Cfat/batch_time - 6.6 * DM_intake/batch_time) * batch_time
   feed_dat.mod$CP_dig <- ((feed_dat.mod$N/batch_time * 0.96 - 8 * DM_intake/batch_time) * 6.25) * batch_time
   feed_dat.mod$starch_dig <- feed_dat.mod$Starch
   feed_dat.mod$sugar_dig <- feed_dat.mod$Sugar
   feed_dat.mod$ResFib_dig <- feed_dat.mod$OM_dig - feed_dat.mod$Cfat_dig - feed_dat.mod$CP_dig - feed_dat.mod$starch_dig - feed_dat.mod$sugar_dig
   
   feed_dat.mod$OM_feces <- feed_dat.mod$OM - feed_dat.mod$OM_dig
   feed_dat.mod$CP_feces <- (feed_dat.mod$N * 6.25) - feed_dat.mod$CP_dig
   feed_dat.mod$Cfat_feces <- feed_dat.mod$Cfat - feed_dat.mod$Cfat_dig
   feed_dat.mod$starch_feces <- feed_dat.mod$Starch - feed_dat.mod$starch_dig # sugar is assumed to be completely digested
   feed_dat.mod$RFd_feces <- feed_dat.mod$ResFib - feed_dat.mod$ResFib_dig - feed_dat.mod$iNDF
   feed_dat.mod$iNDF_feces  <- feed_dat.mod$iNDF
   
   manure <- feed_dat.mod[,  colnames(feed_dat.mod) %in% names.feces]
   manure$ash_feces <- feed_dat.mod$ash 

   ym <- 7.55 - 0.0343 * DM_intake/batch_time - 0.0199 * feed_dat.mod$Cfat/DM_intake - 0.0014 * feed_dat.mod$ash/DM_intake + 0.0028 * feed_dat.mod$NDF/DM_intake - 0.0045 * feed_dat.mod$Starch/DM_intake # percent of GE as methane
   GE <- 24.1 * feed_dat.mod$CP/batch_time + 36.6 * feed_dat.mod$Cfat/batch_time + 18.5 * (feed_dat.mod$OM - feed_dat.mod$CP - feed_dat.mod$Cfat - feed_dat.mod$N * 0)/batch_time # KJ/day
   CH4_ent <- (ym/100 * GE)/55.65 * batch_time # g CH4/batch_time
   # CH4_ent_old <- (76 + 13.5 * DM_intake/batch_time - 9.55 * feed_dat.mod$Cfat/DM_intake + 2.24 * feed_dat.mod$NDF/DM_intake) * batch_time
   
   fermFib <- 0 # ResFib intake feed - (iNDF in feces + RFd in feces) # iNDF in feces = iNDF in feed = Ex_nondeg_ResFib
   sNSP_intake_d <- 0 # kg/day
   sNSP <- 0
   
   
   if(grepl('heifer|cattle 0-6|cattle, tung race, dry|cattle, jersey, dry', type_anim)){
     feces <- (-0.3314 + 2.1819 * DM_intake/batch_time) * batch_time # kg/year
     dm_feces <- (0.1096 + 0.2849 * DM_intake/batch_time) * batch_time # kg/year
    # Johansen et al. Livestock Science 264 (2022) 105058
     urine <- (-16.4644 + 1.0469 * DM_intake/batch_time + 0.6972 * # L/year
       (feed_dat.mod$N / (DM_intake * 1000) * 100) + 27.7501 * 
       (feed_dat.mod$Sodium / (DM_intake * 1000) * 100) + 5.1286 *
       (feed_dat.mod$Potassium / (DM_intake * 1000) * 100)) * batch_time 
     # Johansen et al. Livestock Science 264 (2022) 105058
     urine_N <- (-181.56 + 13.9797 * DM_intake/batch_time + 75.9430 * # g/year
       (feed_dat.mod$N / (DM_intake * 1000) * 100)) * batch_time 
     # Johansen et al. Livestock Science 264 (2022) 105058
     urea_N <- 0.692 * urine_N # g/year
     # Møller
     
     CO2_ent <- 0
     
     if(grepl('tung race', type_anim))
       CO2_ent <- (956 + (122 * DM_intake/batch_time) + (60 * body_weight^0.75) + (3.44 * feed_dat.mod$CP/DM_intake) - 777 + 
                     (206 * DM_intake/batch_time) + (15.7 * DM_intake/batch_time) + (-18.5 * body_weight^0.75)) * batch_time
     
     if(grepl('jersey', type_anim))
       CO2_ent <- (956 + (122 * DM_intake/batch_time) + (60 * body_weight^0.75) + (3.44 * feed_dat.mod$CP/DM_intake) + 1103 + 
                     (204 * DM_intake/batch_time) + (15.7 * DM_intake/batch_time) + (-37.5 * body_weight^0.75)) * batch_time
     
   }
   
   if(grepl('bull calf, 6-slaugter|bull calf, 0-6', type_anim)){
     feces <- (-0.6001 + 1.7689 * DM_intake/batch_time) * batch_time # kg/year
     dm_feces <- (-0.03894 + 0.2817 * DM_intake/batch_time) * batch_time # kg/year
     
     #P.A. Madsen et al. Livestock Science 267 (2023) 105139
     urine <- (-7.2850 + 1.3881 * DM_intake/batch_time - 8.6929 * # L/year
       (feed_dat.mod$N / (DM_intake * 1000) * 100) - 0.7668 * 
       (feed_dat.mod$Sodium / (DM_intake * 1000) * 100) + 14.4688 * 
       (feed_dat.mod$Potassium / (DM_intake * 1000) * 100)) * batch_time
     #Alternative equation, so urine_N is larger than urea_N
     urine_N <- (-95.69 + 10.87 * DM_intake/batch_time + 38.53 * # g/year
       (feed_dat.mod$N / (DM_intake * 1000) * 100)) * batch_time
     #P.A. Madsen et al. Livestock Science 267 (2023) 105139 data
     urea_N <- 0.694 * urine_N 
     #Møller et al. 
     CO2_ent <- 0
   }
   
   if(grepl('cattle, tung race|cattle, jersey', type_anim) & !grepl("cattle, tung race, dry|cattle, jersey, dry", type_anim)){
     feces <- (-1.3548 + 2.2475 * DM_intake/batch_time) * batch_time # kg/year
     dm_feces <- (-0.1143 + 0.2805 * DM_intake/batch_time) * batch_time # kg/year
     
     # BANNINK ET AL. Journal of Dairy Science Vol. 82, No. 5, 1999
     urine <- (1.3441 * DM_intake/batch_time + (1.079 * # L/year
      (feed_dat.mod$Sodium / (DM_intake * 1000) * 100) + 0.5380 * 
      (feed_dat.mod$Potassium / (DM_intake * 1000) * 100) + 0.1266 *
      (feed_dat.mod$N / (DM_intake * 1000) * 100)) - milk_prod/batch_time * 
      (0.1216 + 0.0275 * milk_pro/10)) * batch_time # 
     # NRC (2021) 
     urine_N <- (12 + 0.333 * feed_dat.mod$N/batch_time) * batch_time # g/year
     # from Dijkstra et al., 2013 Animal (2013), 7:s2, pp 292–302
     urea_N <- 0.761 * urine_N # g/year
     
     if(grepl('cattle, tung race', type_anim))
     CO2_ent <- (956 + (122 * DM_intake/batch_time) + (60 * body_weight^0.75) + (3.44 * feed_dat.mod$CP/DM_intake) - 777 + 
       (206 * DM_intake/batch_time) + (15.7 * DM_intake/batch_time) + (-18.5 * body_weight^0.75)) * batch_time
     
     if(grepl('cattle, jersey', type_anim))
     CO2_ent <- (956 + (122 * DM_intake/batch_time) + (60 * body_weight^0.75) + (3.44 * feed_dat.mod$CP/DM_intake) + 1103 + 
       (204 * DM_intake/batch_time) + (15.7 * DM_intake/batch_time) + (-37.5 * body_weight^0.75)) * batch_time
     
   }
 
   manure$VFA_feces <-  0.0287 * dm_feces * 1000
   manure$RFd_feces <- manure$RFd_feces - manure$VFA_feces * 162/105  # subtract VFA from residual fiber
   
  }
 
 
  names <- c(colnames(manure), 'urine_N', 'urea_N', 'feces', 'urine')
  manure <- c(as.numeric(manure), urine_N, urea_N, feces, urine)
  names(manure) <- names
  
  return(list(manure = manure, CH4_ent = CH4_ent, CO2_ent = CO2_ent, fermFib = fermFib/feed_intake))
}