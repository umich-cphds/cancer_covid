vaccine_analysis <- function(reference_level = "0") {
  
  cli::cli_alert_info("severe covid analysis...")
  severe_covid <- vaccine_cascade(outcome = "`Severe COVID`", ref_level = reference_level)
  
  cli::cli_alert_info("hospitalization analysis...")
  hospitalized <- vaccine_cascade(outcome = "Hospitalized", ref_level = reference_level)
  
  cli::cli_alert_info("icu admission analysis...")
  icu          <- vaccine_cascade(outcome = "ICU", ref_level = reference_level)
  
  cli::cli_alert_info("death analysis...")
  death        <- vaccine_cascade(outcome = "Deceased", ref_level = reference_level)
  
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
