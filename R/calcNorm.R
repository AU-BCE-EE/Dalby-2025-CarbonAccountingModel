calcNorm <- function(barn_dat, storage_dat, digestate_dat, ave, days, years, detail_output = FALSE, feed){

  norm_vars <- c('time', 'CH4_emis_rate', 'CO2_emis_rate', 'NH3_emis_rate', 'N2O_emis_rate', 'CH4_A_emis_rate',
                 'N_load_cum', 'C_load_cum', 'COD_load_cum', 'CH4_emis_cum', 'CH4_A_emis_cum', 'CO2_emis_cum', 'NH3_emis_cum', 'N2O_emis_cum',
                 'n_anim', 'class_anim', 'area', 'prod_area', 'slurry_prod_rate', 'evap', 'rain',
                 'slurry_mass', 'slurry_mass_eff', 'N_conc_fresh', 'C_conc_fresh', 
                 'source', 'section_ID', 'storage_ID', 'digestate_ID', 'C_before', 'N_before',
                 'conc_fresh_TAN', 'conc_fresh_urea', 'conc_fresh_VSd_A', 'conc_fresh_VSnd_A', 'C_eff_conc','N_eff_conc', 'VFA_conc', 'RFd_conc',
                 'm2','m1','m0', 'rut_m0','rut_m1','rut_m2',
                 'TAN_eff_conc','urea_eff_conc','wash_water', 'batch_time', 'rest_d', 'MCF_CH4_solid_manure' ,'solid_manure', 'VS_CH4',
                 'CH4_ent', 'CO2_ent', 'temp_C')

  # make separate frame with enteric data
  ent_dat <- barn_dat[, names(barn_dat) %in% norm_vars] %>% 
    mutate(source = 'enteric', CH4_emis_rate = CH4_ent/365 * n_anim, CO2_emis_rate = CO2_ent/365 * n_anim,
           CH4_A_emis_rate = CH4_ent/365 * n_anim, NH3_emis_rate = 0)

  # enteric is 0 when no slurry is produced
  ent_dat$CH4_emis_rate[ent_dat$slurry_prod_rate == 0] <- 0
  ent_dat$CH4_A_emis_rate[ent_dat$slurry_prod_rate == 0] <- 0
  ent_dat$CO2_emis_rate[ent_dat$slurry_prod_rate == 0] <- 0
  ent_dat$NH3_emis_rate[ent_dat$slurry_prod_rate == 0] <- 0
  ent_dat$N2O_emis_rate[ent_dat$slurry_prod_rate == 0] <- 0
  
  ent_dat <- ent_dat %>%
    mutate(
      time_diff = c(0, diff(time)),  # Calculate time difference between consecutive time points
      CH4_emis_rate = CH4_emis_rate * time_diff,
      CH4_A_emis_rate = CH4_A_emis_rate * time_diff,
      CO2_emis_rate = CO2_emis_rate * time_diff,
      NH3_emis_rate = NH3_emis_rate * time_diff,
      N2O_emis_rate = N2O_emis_rate * time_diff,# Calculate emission for each time interval
      CH4_emis_cum = cumsum(CH4_emis_rate),
      CH4_A_emis_cum = cumsum(CH4_A_emis_rate),
      CO2_emis_cum = cumsum(CO2_emis_rate),
      NH3_emis_cum = cumsum(NH3_emis_rate),
      N2O_emis_cum = cumsum(N2O_emis_rate)
      # Cumulative sum of emissions
    )
    
  barn_dat$storage_ID <- NA
  barn_dat$digestate_ID <- NA 

  norm_dat <- bind_rows(ent_dat[, names(ent_dat) %in% norm_vars], barn_dat[, names(barn_dat) %in% norm_vars], storage_dat[, names(storage_dat) %in% norm_vars], digestate_dat[, names(digestate_dat) %in% norm_vars]) %>% 
    mutate(year = ceiling(time/365))
  
  if(detail_output) {
    name <- gsub('../inputs/|/',' ',dat)
    path <- paste0('../outputs/', gsub('xlsm','csv', name))
    write.csv(norm_dat, path)
  }
  
  .rain <- expression(rain[1] * area[1]/1000 * 365) # rain in kg per year
  .evap <- expression(evap[1] * area[1]/1000 * 365) # evaporation in kg per year
  .wash_water <- expression(sum(wash_water/batch_time/1000))
  .slurry_loaded <- expression(sum(slurry_diff[slurry_diff > 0])/1000)
  
  norm_dat <- norm_dat %>% group_by(source, year, section_ID, storage_ID, digestate_ID) %>% mutate(slurry_diff = c(0, diff(slurry_mass)) + (evap - rain) * area)
  
  C_load <- expression((max(C_load_cum) - min(C_load_cum))/1000) # kg/yr
  N_load <- expression((max(N_load_cum) - min(N_load_cum))/1000) # kg/yr
  
  C_CH4 <- 12.0107/16.042
  C_CO2 <- 12.0107/44.009
 # something is wrong with instant here: browser() 
    emission_dat <- norm_dat %>% 
                  group_by(source, year, section_ID, storage_ID, digestate_ID) %>%
                  reframe(CH4_C_kg_yr = (max(CH4_emis_cum, na.rm = T) - min(CH4_emis_cum, na.rm = T))/1000 * C_CH4, CH4_C_kg_anim_yr = (max(CH4_emis_cum, na.rm = T) - min(CH4_emis_cum, na.rm = T))/1000 /n_anim * C_CH4, CH4_C_kg_parea_yr = (max(CH4_emis_cum, na.rm = T) - min(CH4_emis_cum, na.rm = T))/1000 /prod_area * C_CH4, 
                            CH4_C_kg_m3_load_yr = (max(CH4_emis_cum, na.rm = T) - min(CH4_emis_cum, na.rm = T))/1000 /eval(.slurry_loaded) * C_CH4, CH4_C_kg_m3_yr = mean(CH4_emis_rate, na.rm = T)/mean(slurry_mass, na.rm = T) * 365 * C_CH4, 
                            CH4_C_percent_C_load_yr = (max(CH4_emis_cum, na.rm = T) - min(CH4_emis_cum, na.rm = T))/1000/eval(C_load) * C_CH4 * 100,
                            
                            CH4_C_A_kg_yr = (max(CH4_A_emis_cum, na.rm = T) - min(CH4_A_emis_cum, na.rm = T))/1000 * C_CH4, CH4_C_A_kg_anim_yr = (max(CH4_A_emis_cum, na.rm = T) - min(CH4_A_emis_cum, na.rm = T))/1000 /n_anim * C_CH4, CH4_C_A_kg_parea_yr = (max(CH4_A_emis_cum, na.rm = T) - min(CH4_A_emis_cum, na.rm = T))/1000 /prod_area * C_CH4, 
                            CH4_C_A_kg_m3_load_yr = (max(CH4_A_emis_cum, na.rm = T) - min(CH4_A_emis_cum, na.rm = T))/1000 /eval(.slurry_loaded) * C_CH4, CH4_C_A_kg_m3_yr = mean(CH4_A_emis_rate, na.rm = T)/mean(slurry_mass, na.rm = T) * 365 * C_CH4,
                            
                            NH3_N_kg_yr = (max(NH3_emis_cum, na.rm = T) - min(NH3_emis_cum, na.rm = T))/1000,  NH3_N_kg_anim_yr = (max(NH3_emis_cum, na.rm = T) - min(NH3_emis_cum, na.rm = T))/1000 /n_anim, NH3_N_kg_parea_yr = (max(NH3_emis_cum, na.rm = T) - min(NH3_emis_cum, na.rm = T))/1000 /prod_area,
                            NH3_N_kg_m3_load_yr = (max(NH3_emis_cum, na.rm = T) - min(NH3_emis_cum, na.rm = T))/1000 /eval(.slurry_loaded), NH3_N_kg_m3_yr = mean(NH3_emis_rate, na.rm = T)/mean(slurry_mass, na.rm = T) * 365,
                            NH3_N_percent_N_load_yr = (max(NH3_emis_cum, na.rm = T) - min(NH3_emis_cum, na.rm = T))/1000/eval(N_load) * 100,
                            
                            N2O_N_kg_yr = (max(N2O_emis_cum, na.rm = T) - min(N2O_emis_cum, na.rm = T))/1000,  N2O_N_kg_anim_yr = (max(N2O_emis_cum, na.rm = T) - min(N2O_emis_cum, na.rm = T))/1000 /n_anim, N2O_N_kg_parea_yr = (max(N2O_emis_cum, na.rm = T) - min(N2O_emis_cum, na.rm = T))/1000 /prod_area,
                            N2O_N_kg_m3_load_yr = (max(N2O_emis_cum, na.rm = T) - min(N2O_emis_cum, na.rm = T))/1000 /eval(.slurry_loaded), N2O_N_kg_m3_yr = mean(N2O_emis_rate, na.rm = T)/mean(slurry_mass, na.rm = T) * 365,
                            N2O_N_percent_N_load_yr = (max(N2O_emis_cum, na.rm = T) - min(N2O_emis_cum, na.rm = T))/1000/eval(N_load) * 100,
                  
                            CO2_C_kg_yr = (max(CO2_emis_cum, na.rm = T) - min(CO2_emis_cum, na.rm = T))/1000 * C_CO2, CO2_C_kg_anim_yr = (max(CO2_emis_cum, na.rm = T) - min(CO2_emis_cum, na.rm = T))/1000 /n_anim * C_CO2, CO2_C_kg_parea_yr = (max(CO2_emis_cum, na.rm = T) - min(CO2_emis_cum, na.rm = T))/1000 /prod_area * C_CO2,
                            CO2_C_kg_m3_load_yr = (max(CO2_emis_cum, na.rm = T) - min(CO2_emis_cum, na.rm = T))/1000 /eval(.slurry_loaded) * C_CO2, CO2_C_kg_m3_yr = mean(CO2_emis_rate, na.rm = T)/mean(slurry_mass, na.rm = T) * 365 * C_CO2,
                            CO2_C_percent_C_load_yr = (max(CO2_emis_cum, na.rm = T) - min(CO2_emis_cum, na.rm = T))/1000/eval(C_load) * C_CO2 * 100) %>% 
                            
                            filter (year == years-1) %>% distinct() %>% ungroup() %>% 
                            mutate(across(starts_with(c('CH4','NH3', 'N2O', 'CO2','slurry_')), function(x) round(x, digits = 2))) %>% 
                            mutate(source = factor(source, levels = c('enteric','barn','storage'))) %>%
                            arrange(source)

    nutrient_dat <- norm_dat %>% filter(!source %in% c('enteric')) %>% 
                  group_by(source, year, section_ID, storage_ID, digestate_ID) %>%
                  summarise(C_load_kg_yr =  eval(C_load), 
                  C_emit_kg_yr = ((max(CH4_emis_cum) - min(CH4_emis_cum)) * C_CH4 + (max(CO2_emis_cum) - min(CO2_emis_cum)) * C_CO2)/1000,
                  N_load_kg_yr = eval(N_load), N_emit_kg_yr = ((max(NH3_emis_cum) - min(NH3_emis_cum)) + (max(N2O_emis_cum) - min(N2O_emis_cum)))/1000)
                        
    enteric_nutrient <- reframe(group_by(norm_dat[norm_dat$source == 'enteric',], source, year, section_ID, storage_ID, digestate_ID),
                      C_load_kg_yr = (feed['Starch'] * 0.444 + feed['Sugar'] * 0.421 + feed['ResFib'] * 0.444 + feed['CP'] * 0.550 + feed['Cfat'] * 0.759) * feed['feed_intake'] * unique(n_anim)/1000,
                      C_emit_kg_yr = emission_dat$CH4_C_kg_yr[emission_dat$source == 'enteric'] + emission_dat$CO2_C_kg_yr[emission_dat$source == 'enteric'],
                      N_load_kg_yr = feed['CP'] * 0.16 * feed['feed_intake'] * unique(n_anim)/1000,
                      N_emit_kg_yr = 0) %>% arrange(source) %>% ungroup()
    enteric_nutrient$storage_ID <- NA
    enteric_nutrient$digestate_ID <- NA
    enteric_nutrient$source <- 'animal'
  
    nutrient_dat <- rbind(enteric_nutrient, nutrient_dat)
    nutrient_dat <- nutrient_dat %>% group_by(source, year, section_ID, storage_ID, digestate_ID) %>%
      mutate(C_loss_frac = C_emit_kg_yr/C_load_kg_yr, N_loss_frac = N_emit_kg_yr/N_load_kg_yr) %>% filter(year == years-1)
    nutrient_dat[is.na(nutrient_dat)] <- 0

  
  production_dat <- norm_dat %>%
    filter(!source %in% c('enteric')) %>%
    group_by(source, year, section_ID, storage_ID, digestate_ID) %>%
    summarise(slurry_loaded = eval(.slurry_loaded), rain = eval(.rain), 
              evaporation = eval(.evap), wash_water = eval(.wash_water)) %>%
    mutate(across(everything(), function(x) round(x, 1))) %>%
    filter(year == years - 1)
  
  
  explore_vars <- c('source', 'year', 'section_ID', 'storage_ID', 'digestate_ID',
                    'cum_inhib_m0', 'HAC_inhib_m0', 'H2S_inhib_m0', 'H2S_inhib_sr1', 'NH4_inhib_m0', 'NH3_inhib_m0', 'H2SO4_inhib_m0', 
                    'xa_fresh_m0', 'xa_fresh_m1', 'xa_fresh_m2', 'xa_fresh_sr1', 
                    'm0_conc', 'm1_conc', 'm2_conc','sr1_conc',
                    'conc_fresh_urea', 'conc_fresh_sulfate', 'conc_fresh_sulfide', 
                    'conc_fresh_TAN', 'conc_fresh_VFA', 'conc_fresh_CP', 
                    'conc_fresh_Cfat', 'conc_fresh_RFd', 'conc_fresh_starch', 'conc_fresh_iNDF',
                    'conc_fresh_VSd',
                    'conc_fresh_VSd_A', 'conc_fresh_VSnd_A', 'conc_fresh_xa_dead',
                    'COD_conc_fresh', 'dCOD_conc_fresh', 
                    'CH4_emis_rate_slurry', 'time')
  
  explore_dat <- bind_rows(barn_dat[, names(barn_dat) %in% explore_vars], storage_dat[, names(storage_dat) %in% explore_vars], digestate_dat[, names(digestate_dat) %in% explore_vars]) %>% 
    mutate(year = ceiling(time/365))
  
  expl_dat <- explore_dat %>% 
    group_by(source, year, section_ID, storage_ID, digestate_ID) %>%
    summarise(across(, .fns = ~ mean(.x, na.rm = TRUE))) %>% 
    mutate(across(everything(), function(x) round(x,2))) %>% 
    select(-time) %>% filter(year == years-1) %>% 
    t() %>% as.data.frame() %>%
    mutate(across(where(is.character), as.numeric))
    colnames(expl_dat) <- c(paste0('section ', unique(explore_dat$section_ID[!is.na(explore_dat$section_ID)])), paste0('storage ',unique(explore_dat$storage_ID[!is.na(explore_dat$storage_ID)])))
  
  # calculate methane from deep litter with MCF factor.
    CH4_kg_solid <- norm_dat %>% filter(source == 'barn') %>% 
    mutate(VSd_kg_load_yr = solid_manure * 365/(batch_time + rest_d) * (conc_fresh_VSd_A/1000),
           solid_manure_m3_load_yr = solid_manure * 365/(batch_time + rest_d)/1000) %>% 
    group_by(year, section_ID) %>%
    summarise(CH4_C_kg_yr = mean(VSd_kg_load_yr, na.rm = T) / mean(VS_CH4, na.rm = T) * mean(MCF_CH4_solid_manure, na.rm = T)/100 * C_CH4,
              n_anim = mean(n_anim), solid_manure_m3_load_yr = mean(solid_manure_m3_load_yr), prod_area = mean(prod_area)) %>% mutate(CH4_C_kg_anim_yr = CH4_C_kg_yr/n_anim,
                                          CH4_C_kg_m3_load_yr = CH4_C_kg_yr / solid_manure_m3_load_yr, 
                                          CH4_C_kg_parea_yr = CH4_C_kg_yr / prod_area,
                                          source = 'solid') %>% filter(year == years-1) %>% select(-n_anim, -solid_manure_m3_load_yr)
  # bind with emission data and sum up. 
  emission_dat <- merge(emission_dat, CH4_kg_solid, all = T) %>% select(source, year, section_ID, storage_ID, digestate_ID, everything())                      
  emission_dat[is.na(emission_dat)] <- 0
  emission_dat <- emission_dat[, !colnames(emission_dat) %in% c('prod_area','year')]
  #emission_dat <- data.frame(emission_dat)
  #browser()
  
  setDT(emission_dat)
  emission_long <- melt(emission_dat, measure.vars = names(emission_dat)[!names(emission_dat) %in% c('source','section_ID','storage_ID','digestate_ID')])
  
  allowed_strings <- c('CH4_C', 'CO2_C', 'NH3', 'N2O')
  
  # Create a regex pattern to match only the desired substrings
  pattern <- paste0(".*(", paste(allowed_strings, collapse = "|"), ").*")
  
  # Extract the desired part or replace with NA if no match
  emission_long$gas <- ifelse(grepl(pattern, emission_long$variable),
                            sub(pattern, "\\1", emission_long$variable),
                            NA)
  emission_long$gas <- gsub('_C','-C', emission_long$gas)
  emission_long$gas <- gsub('NH3','NH3-N', emission_long$gas)
  emission_long$gas <- gsub('N2O','N2O-N', emission_long$gas)

  return(list('Emission data' = as.data.frame(emission_long), 'Nutrient balance' = nutrient_dat))
}  

