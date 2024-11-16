mitigate_storage <- function(specs, from, storage){
  
  if(from == 'storage'){
    CH4_ox <- storage$CH4_emis_rate * (specs$venting_s * specs$venting_eff_s/100) 
    CH4_flare <- (storage$CH4_emis_rate - CH4_ox) * (specs$flaring_s * specs$flaring_eff_s/100)
    storage$CO2_emis_rate <- storage$CO2_emis_rate + (CH4_ox + CH4_flare) * 44.01/16.04
    storage$CH4_emis_rate <- storage$CH4_emis_rate - (CH4_ox + CH4_flare) 
    CH4_A_ox <- storage$CH4_A_emis_rate * (specs$venting_s * specs$venting_eff_s/100) 
    CH4_A_flare <- (storage$CH4_A_emis_rate - CH4_ox) * (specs$flaring_s * specs$flaring_eff_s/100)
    storage$CH4_A_emis_rate <- storage$CH4_A_emis_rate - (CH4_A_ox + CH4_A_flare)  
  }
  
  if(from == 'digestate'){
    CH4_ox <- storage$CH4_emis_rate * (specs$venting_d * specs$venting_eff_d/100) 
    CH4_flare <- (storage$CH4_emis_rate - CH4_ox) * (specs$flaring_d * specs$flaring_eff_d/100)
    storage$CO2_emis_rate <- storage$CO2_emis_rate + (CH4_ox + CH4_flare) * 44.01/16.04
    storage$CH4_emis_rate <- storage$CH4_emis_rate - (CH4_ox + CH4_flare) 
    CH4_A_ox <- storage$CH4_A_emis_rate * (specs$venting_d * specs$venting_eff_d/100) 
    CH4_A_flare <- (storage$CH4_A_emis_rate - CH4_ox) * (specs$flaring_d * specs$flaring_eff_d/100)
    storage$CH4_A_emis_rate <- storage$CH4_A_emis_rate - (CH4_A_ox + CH4_A_flare)
  }
  
  return(storage)
  
}