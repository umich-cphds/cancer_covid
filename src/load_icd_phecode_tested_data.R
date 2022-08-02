# load ICD phecode data ---------
load_icd_phecode_tested_data <- function(icd9_path, icd10_path) {

  # ICD 9 codes in tested/dx cohort
  load(icd9_path)
  diagnoses_ICD9 <- unique(diagnoses_ICD9)[, lexicon := "ICD9"]
  cli::cli_alert_success("ICD-9 data loaded")
  
  # ICD 10 codes in tested/dx cohort
  load(icd10_path)
  diagnoses_ICD10 <- unique(diagnoses_ICD10)[, lexicon := "ICD10"]
  cli::cli_alert_success("ICD-10 data loaded. stacking...")
  
  stacked <- rbindlist(list(
    diagnoses_ICD9,
    diagnoses_ICD10
  ))[, tested := 1][]
  
  setnames(stacked, "IID", "id")
  
  return(stacked)
  
}
