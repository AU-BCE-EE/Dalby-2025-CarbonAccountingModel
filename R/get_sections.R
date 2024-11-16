get_sections <- function(sections = sections){
  
  class_anim <- sections[['animal_class']] # need for evaluating feed unit. 
  
  section_parms <- list(class_anim = sections[['animal_class']],
  type_anim =      sections[['animal_category']],
  n_anim =         sections[['animals,_section-1']],
  floor_type =     sections[['floor_type']],
  removal_tech =   sections[['removal_technology']],
  
  # management parms
  vent =           sections[['ventilation']],
  temp_air_C =     sections[['barn_temperature,_deg_C']],
  storage_depth =  sections[['pit_depth,_cm']]/100,
  empty_int =      sections[['slurry_removal_frequency,_days']],
  resid_depth =    sections[['residual_slurry_depth,_cm']]/100,
  wash_int =       ifelse(sections[['wash_frequency,_days']] == "NA", NA, sections[['wash_frequency,_days']]),
  wash_water =     sections[['wash_water,_kg_animal-1']],
  rest_d =         sections[['empty_time_of_section,_days']],
  graze      =     list(start =sections[['grazing_start,_month']], duration = sections[['grazing_days,_days_yr-1']], hours_day = sections[['grazing_hours,_h_day-1']]),
  excretion_ratio_deep_litter = sections[['excretion_ratio,_deeplitter/total']],
  pit_floor_ratio = sections[['pit/floor_ratio']],
  spec_area =      sections[['area,_m2_animal-1']],
  prod_area =      sections[['production_area,_m2_section-1']],
  area =           sections[['pit_area,_m2_section-1']],

  # feed and growth parms
  batch_time =     sections[['production_time,_days']],
  bedding =        sections[["bedding_material,_kg_animal-1"]],
  bedding_TS =      sections[['total_solids_in_bedding_material,_%']],
  feed_intake =    ifelse(class_anim == "pig", sections[["feed_intake,_kg_animal-1"]], sections[["feed_intake,_kg_DM_animal-1_yr-1"]]),
  feed_spill_frac = sections[['feed_spill_fraction,_%']],
  milk_prod =      sections[["milk_production,_kg_animal-1_yr-1"]],
  milk_pro =       sections[["milk_protein,_g_kg_milk-1"]],
  body_weight =    sections[['body_weight,_kg_animal-1']],
  spec_urine =     sections[['urine_production,_kg_kg_feed_TS-1,_kg_kg_feces-1']],
  
  # feed composition parms
  sum_feed =       sections[["sum_feed"]],
  
  # technologies parms
  barn_acid =      sections[["acidification"]],
  barn_acid_dose = sections[["acid_dose,_kg_m-3"]],
  solid_liquid =   sections[["solid_liquid_separation"]],
  removal_SLS =    sections[['VS_removal,_%']],
  barn_cool =      sections[["slurry_cooling"]],
  cool_eff =       sections[['cooling_effect,_w_m-2']],
  cool_days =      sections[['cooling_time,_days_yr-1']],
  biogas =         sections[["biogas"]],
  f_ex_storage =   sections[['fraction_to_storage']],
  f_biogas =       sections[['fraction_to_biogas']],
  removal_biogas = sections[['COD_removal,_%']],
  
  # Arrhenius parms
  lnA =            sections[["LnA"]],
  E_CH4 =          sections[["Ea"]],
  VS_CH4 =         sections[['VS/CH4']],
  VSd_VS =         sections[['VS_degradability']],

  
  # other  
  digestate_tank_ID = sections[['digestateID']],
  storage_tank_ID = sections[['storageID']],
  kl_NH3_pit = sections[['MTC_NH3_pit']],
  kl_NH3_floor = sections[['MTC_NH3_floor']],
  MCF_CH4_solid_manure = sections[['MCF_CH4_solid_manure,_%']])
  
  if(any(is.null(sections))) stop('NULL in section variables')
  
  return(section_parms)
  
}