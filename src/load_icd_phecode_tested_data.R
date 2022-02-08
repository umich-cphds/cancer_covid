# load ICD phecode data ---------
load_icd_phecode_tested_data <- function(cohort = "20220101") {
  
  if (cohort == "20220101") {
    icd9_path  <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220101/phenomes/UNFILTERED_20220101/UNFILTERED_20220101_ICD9_Phecodes_Birthyears.Rsav"
    icd10_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220101/phenomes/UNFILTERED_20220101/UNFILTERED_20220101_ICD10_Phecodes_Birthyears.Rsav"
  }
  
  if (cohort == "20211001"){
    icd9_path  <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20211001/phenomes/UNFILTERED_20211001/UNFILTERED_20211001_ICD9_Phecodes_Birthyears.Rsav"
    icd10_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20211001/phenomes/UNFILTERED_20211001/UNFILTERED_20211001_ICD10_Phecodes_Birthyears.Rsav"
  }
  
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
