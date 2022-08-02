interaction_analysis <- function(dataset,
                                 env_name = .GlobalEnv,
                                 exposures = c("Age", "AgeCategory", "Sex", "RaceEthnicity4", "BMI",
                                               "BMIcategory", "Drinker", "SmokingStatus",
                                               "popden13_17_qrtl", "disadvantage2_13_17_qrtl", "affluence13_17_qrtl",
                                               "ethnicimmigrant13_17_qrtl", "ped1_13_17_qrtl", "ComorbidityScore"),
                                 comorbidities = c("RespiratoryDiseases", "CirculatoryDiseases", "Type2Diabetes",
                                                   "KidneyDiseases", "LiverDiseases", "AutoimmuneDiseases"),
                                 interaction_var = "AnyCancerPhe",
                                 reference_level = "0") {
  
  # interaction models ----------
  sev_int_mods <- list()
  hos_int_mods <- list()
  icu_int_mods <- list()
  dea_int_mods <- list()
  
  for (i in seq_along(exposures)) {
    
    tmp_covariates <- c(adj_sets[["adj3"]], interaction_var)[!grepl(exposures[i], c(adj_sets[["adj3"]], interaction_var))]
    # tmp_covariates <- c(adj_sets[["adj3"]], interaction_var)[c(adj_sets[["adj3"]], interaction_var) %notin% exposures[i]]
    
    cli::cli_alert("fitting {exposures[i]} exposure model")
    
    sev_int_mods[[exposures[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = exposures[i],
      outcome    = "`Severe COVID`",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var,
      ref        = reference_level
    )
    
    hos_int_mods[[exposures[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = exposures[i],
      outcome    = "Hospitalized",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var,
      ref        = reference_level
    )
    
    icu_int_mods[[exposures[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = exposures[i],
      outcome    = "ICU",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var,
      ref        = reference_level
    )
    
    dea_int_mods[[exposures[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = exposures[i],
      outcome    = "Deceased",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var,
      ref        = reference_level
    )
    
  }
  
  # comorbidity models ---------
  sev_comorbid_mods <- list()
  hos_comorbid_mods <- list()
  icu_comorbid_mods <- list()
  dea_comorbid_mods <- list()
  
  for (i in seq_along(comorbidities)) {
    
    cli::cli_alert("fitting {comorbidities[i]} exposure model")
    
    tmp_covariates <- c(adj_sets[["adj3"]], interaction_var)[!grepl(comorbidities[i], c(adj_sets[["adj3"]], interaction_var))]
    
    sev_comorbid_mods[[comorbidities[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = comorbidities[i],
      outcome    = "`Severe COVID`",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var,
      ref        = reference_level
    )
    
    hos_comorbid_mods[[comorbidities[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = comorbidities[i],
      outcome    = "Hospitalized",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var,
      ref        = reference_level
    )
    
    icu_comorbid_mods[[comorbidities[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = comorbidities[i],
      outcome    = "ICU",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var,
      ref        = reference_level
    )
    
    dea_comorbid_mods[[comorbidities[i]]] <- interaction_model(
      dataset    = dataset,
      exposure   = comorbidities[i],
      outcome    = "Deceased",
      covariates = tmp_covariates,
      env_name   = env_name,
      int        = interaction_var,
      ref        = reference_level
    )
    
  }
  
  return(
    list(
      sev_int_mods      = sev_int_mods,
      hos_int_mods      = hos_int_mods,
      icu_int_mods      = icu_int_mods,
      dea_int_mods      = dea_int_mods,
      sev_comorbid_mods = sev_comorbid_mods,
      hos_comorbid_mods = hos_comorbid_mods,
      icu_comorbid_mods = icu_comorbid_mods,
      dea_comorbid_mods = dea_comorbid_mods
    )
  )
  
}