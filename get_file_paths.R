get_file_paths <- function(cohort) {
  
  cli::cli_alert_info(paste0("loading data for `", cohort, "` cohort"))
  
  comorbid_path <- "/net/wonderland/home/mmsalva/projects/covid/new_cancer/lists/comorbidities.rds"
  
  if (cohort == "20220701") {
    cov_path      <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220701/MichiganMedicine_PCRcohort_plus_VaccinationData_20220701.Rsav"
    proc_path     <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883/20220701/Procedures_2022-07-02.txt"
    icd9_path     <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220701/phenomes/UNFILTERED_20220701/UNFILTERED_20220701_ICD9_Phecodes_Birthyears.Rsav"
    icd10_path    <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220701/phenomes/UNFILTERED_20220701/UNFILTERED_20220701_ICD10_Phecodes_Birthyears.Rsav"
  }
  
  if (cohort == "20220202") {
    cov_path      <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220202/MichiganMedicine_PCRcohort_plus_VaccinationData_20220202.Rsav"
    proc_path     <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883/20220202/Procedures_2022-02-02.txt"
    icd9_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220202/phenomes/UNFILTERED_20220202/UNFILTERED_20220202_ICD9_Phecodes_Birthyears.Rsav"
    icd10_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220202/phenomes/UNFILTERED_20220202/UNFILTERED_20220202_ICD10_Phecodes_Birthyears.Rsav"
  }
  
  if (cohort == "20220101") {
    icd9_path  <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220101/phenomes/UNFILTERED_20220101/UNFILTERED_20220101_ICD9_Phecodes_Birthyears.Rsav"
    icd10_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220101/phenomes/UNFILTERED_20220101/UNFILTERED_20220101_ICD10_Phecodes_Birthyears.Rsav"
  }
  
  if (cohort == "20211001"){
    icd9_path  <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20211001/phenomes/UNFILTERED_20211001/UNFILTERED_20211001_ICD9_Phecodes_Birthyears.Rsav"
    icd10_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20211001/phenomes/UNFILTERED_20211001/UNFILTERED_20211001_ICD10_Phecodes_Birthyears.Rsav"
  }
  
  return(list(
    covariates    = cov_path,
    procedures    = proc_path,
    comorbidities = comorbid_path,
    icd9          = icd9_path,
    icd10         = icd10_path
  ))
  
}
