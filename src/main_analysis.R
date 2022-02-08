main_analysis <- function(dataset, exposure_var = "AnyCancerPhe") {
  
  hospitalized <- cascade_analysis(
    data       = dataset,
    exposure   = exposure_var,
    outcome    = "Hospitalized"
  )
  
  icu          <- cascade_analysis(
    data       = dataset,
    exposure   = exposure_var,
    outcome    = "ICU"
  )
  
  death        <- cascade_analysis(
    data       = dataset,
    exposure   = exposure_var,
    outcome    = "Deceased"
  )

  tidy_hosp  <- extract_cascade_estimates(results = hospitalized, outcome = "Hospitalized")
  tidy_icu   <- extract_cascade_estimates(results = icu, outcome = "ICU")
  tidy_death <- extract_cascade_estimates(results = death, outcome = "Deceased")
  
  list(
    hospitalized = tidy_hosp,
    icu          = tidy_icu,
    deceased     = tidy_death,
    clean        = tidy_hosp$tidy %>%
      left_join(tidy_icu$tidy, by = "model") %>%
      left_join(tidy_death$tidy, by = "model")
  )
  
  
}