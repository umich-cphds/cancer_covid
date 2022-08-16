make_processed_phecode_data <- function(cohort = "20220801", fl_pths) {
  
  if (file.exists(paste0("objects/", cohort, "/processed_phecode_data_", cohort, ".fst"))) {
    message(paste0("File already exists... objects/", cohort, "/processed_phecode_data_", cohort, ".fst"))
    icd_phecode <- fst::read_fst(paste0("objects/", cohort, "/processed_phecode_data_", cohort, ".fst"), as.data.table = TRUE)
  } else {
    
    # paths -------------
    cov_path      <- fl_pths$covariates
    # proc_path     <- fl_pths$procedures
    comorbid_path <- fl_pths$comorbidities
    
    # load phecode data ----------
    icd_phecode_tested <- load_icd_phecode_tested_data(
      icd9_path  = fl_pths$icd9,
      icd10_path = fl_pths$icd10
    )
    icd_phecode        <- sex_concordance_check(x = icd_phecode_tested, tested = TRUE)
    
    fst::write_fst(icd_phecode, paste0("objects/", cohort, "/processed_phecode_data_", cohort, ".fst"), compress = 100)
  }
  
  return(icd_phecode)
  
}