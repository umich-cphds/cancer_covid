main_analysis <- function(dataset, exposure_var = "AnyCancerPhe", interaction_var = NULL, model_term = NULL) {
  
  if (is.null(model_term)) {
    model_term <- rep(NA, length(exposure_var))
    for (i in seq_along(exposure_var)) {
      if (!grepl("factor\\(", exposure_var[i])) {
        model_term[i] <- paste0("factor(", exposure_var[i], ")")
      } else {
        model_term[i] <- exposure_var[i]
      }
    }
    # model_term <- paste0("factor\\(", exposure_var, "\\)")
  }
  
  
  cli::cli_alert_info("severe covid analysis...")
  severe_covid <- type_cascade_analysis(
    data       = dataset,
    exposure   = exposure_var,
    outcome    = "`Severe COVID`",
    int        = interaction_var
  )
  
  cli::cli_alert_info("hospitalization analysis...")
  hospitalized <- type_cascade_analysis(
    data       = dataset,
    exposure   = exposure_var,
    outcome    = "Hospitalized",
    int        = interaction_var
  )
  
  cli::cli_alert_info("icu admission analysis...")
  icu          <- type_cascade_analysis(
    data       = dataset,
    exposure   = exposure_var,
    outcome    = "ICU",
    int        = interaction_var
  )
  
  cli::cli_alert_info("death analysis...")
  death        <- type_cascade_analysis(
    data       = dataset,
    exposure   = exposure_var,
    outcome    = "Deceased",
    int        = interaction_var
  )

  cli::cli_alert_info("tidying up...")
  tidy_severe <- extract_cascade_estimates(results = severe_covid, outcome = "Severe COVID", mod_term = model_term)
  tidy_hosp  <- extract_cascade_estimates(results = hospitalized, outcome = "Hospitalized", mod_term = model_term)
  tidy_icu   <- extract_cascade_estimates(results = icu, outcome = "ICU", mod_term = model_term)
  tidy_death <- extract_cascade_estimates(results = death, outcome = "Deceased", mod_term = model_term)
  
  list(
    severe_covid = tidy_severe,
    hospitalized = tidy_hosp,
    icu          = tidy_icu,
    deceased     = tidy_death,
    clean        = tidy_severe$tidy %>%
      left_join(tidy_hosp$tidy, by = c("model", "term"))  %>%
      left_join(tidy_icu$tidy, by = c("model", "term")) %>%
      left_join(tidy_death$tidy, by = c("model", "term"))
  )
  
  
}