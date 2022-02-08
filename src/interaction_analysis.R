interaction_analysis <- function(dataset,
                                 env_name = .GlobalEnv,
                                 exposures = c("Age", "AgeCategory", "Sex", "RaceEthnicity4", "BMI",
                                               "BMIcategory", "Drinker", "SmokingStatus",
                                               "popden13_17_qrtl", "disadvantage2_13_17_qrtl", "affluence13_17_qrtl",
                                               "ethnicimmigrant13_17_qrtl", "ped1_13_17_qrtl", "ComorbidityScore"),
                                 comorbidities = c("RespiratoryDiseases", "CirculatoryDiseases", "Type2Diabetes",
                                                   "KidneyDiseases", "LiverDiseases", "AutoimmuneDiseases"),
                                 interaction_var = "AnyCancerPhe") {
  
  # interaction models ----------
  hos_int_mods <- list()
  icu_int_mods <- list()
  
  for (i in seq_along(exposures)) {
    
    tmp_covariates <- c(adj_sets[["adj3"]], interaction_var)[c(adj_sets[["adj3"]], interaction_var) %notin% exposures[i]]
    
    cli::cli_alert("fitting {exposures[i]} exposure model")
    
    hos_int_mods[[exposures[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = exposures[i],
      outcome    = "Hospitalized",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var
    )
    
    icu_int_mods[[exposures[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = exposures[i],
      outcome    = "ICU",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var
    )
    
  }
  
  # comorbidity models ---------
  hos_comorbid_mods <- list()
  icu_comorbid_mods <- list()
  
  for (i in seq_along(comorbidities)) {
    
    cli::cli_alert("fitting {comorbidities[i]} exposure model")
    
    tmp_covariates <- c(adj_sets[["adj3"]], interaction_var)[c(adj_sets[["adj3"]], interaction_var) %notin% comorbidities[i]]
    
    hos_comorbid_mods[[comorbidities[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = comorbidities[i],
      outcome    = "Hospitalized",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var
    )
    
    icu_comorbid_mods[[comorbidities[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = comorbidities[i],
      outcome    = "ICU",
      covariates = c(adj_sets[["adj3"]], interaction_var),
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var
    )
    
  }
  
  return(
    list(
      hos_int_mods      = hos_int_mods,
      icu_int_mods      = icu_int_mods,
      hos_comorbid_mods = hos_comorbid_mods,
      icu_comorbid_mods = icu_comorbid_mods
    )
  )
  
}