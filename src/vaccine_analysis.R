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
  tidy_severe <- quick_vax_extract(x = severe_covid)
  tidy_hosp   <- quick_vax_extract(x = hospitalized)
  tidy_icu    <- quick_vax_extract(x = icu)
  tidy_death  <- quick_vax_extract(x = death)
  
  list(
    severe_covid = tidy_severe,
    hospitalized = tidy_hosp,
    icu          = tidy_icu,
    deceased     = tidy_death,
    clean        = tidy_severe$tidy %>%
      left_join(tidy_hosp$tidy, by = c("term"))  %>%
      left_join(tidy_icu$tidy, by = c("term")) %>%
      left_join(tidy_death$tidy, by = c("term"))
  )
  
}
