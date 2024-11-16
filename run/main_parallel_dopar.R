rm(list = ls())

# get libraries
library(doParallel)
library(ABM)
library(dplyr)
library(readxl)
library(openxlsx)

# get data file paths
#path <- '../inputs/saman_paper/'

path <- '../inputs/'

dat <- list.files(path, full.names = TRUE)
dat <- dat[grepl('xlsm', dat)]

gf <- list.files('../R', full.names = TRUE) 
for (f in gf) source(f)

storage_mode <- TRUE
years <- 4
temp_overwrite = 'Vechi'
cores <- 1

# write function for parallel programming

cl <- makeCluster(cores)
doParallel::registerDoParallel(cl)

result <- foreach(i = 1:length(dat), .inorder = FALSE, .errorhandling = 'remove', .export = ls(globalenv()),
        .packages = c('dplyr','readxl','ABM','openxlsx')) %dopar% {

  model(x,y, dat = dat[i], storage_mode, years, temp_overwrite = temp_overwrite)
  
}

stopCluster(cl)

output.mod <- do.call(rbind, lapply(result, function(x){
  emis <- data.frame(source = x[['emission_dat']]$source,
                      CH4 = x[['emission_dat']]$CH4_C_kg_anim_yr, 
                      CH4_A = x[['emission_dat']]$CH4_C_A_kg_anim_yr, 
                      NH3 = x[['emission_dat']]$NH3_N_kg_anim_yr,
                      N2O = x[['emission_dat']]$N2O_N_kg_anim_yr,
                      VFA_conc_fresh_anim = x[['expl_dat']][['V1']][[16]],
                      starch_conc_fresh_anim = x[['expl_dat']][['V1']][[15]],
                      RFd_conc_fresh_anim = x[['expl_dat']][['V1']][[20]],
                      CP_conc_fresh_anim = x[['expl_dat']][['V1']][[19]],
                      Cfat_conc_fresh_anim = x[['expl_dat']][['V1']][[18]],
                      iNDF_conc_fresh_anim = x[['expl_dat']][['V1']][[21]],
                      urea_conc_fresh_anim = x[['expl_dat']][['V1']][[12]],
                      TAN_conc_fresh_anim = x[['expl_dat']][['V1']][[14]],
                      VFA_conc_fresh_barn = x[['expl_dat']][['V2']][[16]],
                      starch_conc_fresh_barn = x[['expl_dat']][['V2']][[15]],
                      RFd_conc_fresh_barn = x[['expl_dat']][['V2']][[20]],
                      CP_conc_fresh_barn = x[['expl_dat']][['V2']][[19]],
                      Cfat_conc_fresh_barn = x[['expl_dat']][['V2']][[18]],
                      iNDF_conc_fresh_anim = x[['expl_dat']][['V2']][[21]],
                      urea_conc_fresh_anim = x[['expl_dat']][['V2']][[12]],
                      TAN_conc_fresh_anim = x[['expl_dat']][['V2']][[14]],
                      cum_inhib_barn = x[['expl_dat']][['V1']][[10]],
                      NH4_inhib_barn = x[['expl_dat']][['V1']][[9]],
                      H2S_inhib_barn = x[['expl_dat']][['V1']][[6]],
                      cum_inhib_storage = x[['expl_dat']][['V2']][[10]],
                      NH4_inhib_storage = x[['expl_dat']][['V2']][[9]],
                      H2S_inhib_storage = x[['expl_dat']][['V2']][[6]],
                      feces = x[['manure']]$manure[[10]],
                      urine = x[['manure']]$manure[[11]],
                      bedding = x[['manure']]$manure[[12]],
                      feed_spill = x[['manure']]$manure[[13]],
                      slurry = x[['manure']]$manure[[14]],
                      CP_feces = x[['manure']]$manure[[1]],
                      Cfat_feces = x[['manure']]$manure[[2]],
                      RFd_feces = x[['manure']]$manure[[3]],
                      iNDF_feces = x[['manure']]$manure[[4]],
                      starch_feces = x[['manure']]$manure[[5]],
                      VFA_feces = x[['manure']]$manure[[7]],
                      urine_N = x[['manure']]$manure[[8]],
                      urea_N = x[['manure']]$manure[[9]],
                      evap_water = x[['production_dat']]$evaporation[[1]],
                      wash_water = x[['production_dat']]$wash_water[[1]],
                      NSP_feed = x[['feed']]$feed[[1]],
                      sNSP_feed = x[['feed']]$feed[[2]],
                      OM_feed = x[['feed']]$feed[[3]],
                      CP_feed = x[['feed']]$feed[[4]],
                      Cfat_feed = x[['feed']]$feed[[5]],
                      NDF_feed = x[['feed']]$feed[[6]],
                      Starch_feed = x[['feed']]$feed[[7]],
                      Sugar_feed = x[['feed']]$feed[[8]],
                      ResFib_feed = x[['feed']]$feed[[9]],
                      iNDF_feed = x[['feed']]$feed[[10]],
                      DM_feed = x[['feed']]$feed[[11]],
                      feed_intake = x[['feed']]$feed[[14]],
                      RFd_feed = x[['feed']]$feed[[15]],
                      FF_feed = x[['feed']]$feed[[16]],
                      production_time = x[['input_barn']]$`Value 1`[[30]],
                      empty_time = x[['input_barn']]$`Value 1`[[19]],
                      file = x[[10]]
                      )}))

write.csv(output.mod, '../outputs/full_output.csv', row.names = F)
write.xlsx(output.mod, '../outputs/full_output.xlsx')

