# load covariate data ----------
get_threshold_date <- function(cohort = "20220202") {
  
  if (cohort == "20220202") {
    cov_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220202/MichiganMedicine_PCRcohort_plus_VaccinationData_20220202.Rsav"
  }
  if (cohort == "20220101") {
    cov_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220101/MichiganMedicine_PCRcohort_plus_VaccinationData_20220101.Rsav"
  }
  
  if (cohort == "20211001") {
    cov_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20211001/MichiganMedicine_PCRcohort_plus_VaccinationData_20211001.Rsav"
  }
  
  load(cov_path)
  
  pos <- PCRcohort[COV19_positiveYN == 1, .(id = Encrypted_PatientID, threshold_dsb = DaysSinceBirth_First_PositiveTestDiagnosis, COV19_positiveYN)][, threshold_dsb := threshold_dsb - 14][!is.infinite(threshold_dsb)]
  
  neg <- PCRcohort[COV19_positiveYN == 0, .(id = Encrypted_PatientID, threshold_dsb = DaysSinceBirth_First_Test_Diagnosis, COV19_positiveYN)][, threshold_dsb := threshold_dsb - 14][!is.infinite(threshold_dsb)]
  
  out <- rbindlist(list(
    pos,
    neg
  ))

  return(out)
  
}

# threshold <- get_threshold_date()
# 
# d <- merge.data.table(
#   icd_phecode_tested,
#   threshold,
#   all.x = TRUE,
#   by = "id"
# )
# 
# d[]