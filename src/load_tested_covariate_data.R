# load covariate data ----------
load_tested_covariate_data <- function(outvar, tested_phenome_ids, cancer_tested_ids, cohort = "20220202") {
  
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
  
  # Test Result
  PCRcohort[,OutcomeMax := fifelse(COV19_positiveYN == 0, "N", "P")]
  
  # died within 2 months after first pos test
  PCRcohort[COV19_positiveYN == 1 &
              data.table::between(
                x     = Deceased_DaysSinceBirth,
                lower = DaysSinceBirth_First_PositiveTestDiagnosis - 14,
                upper = DaysSinceBirth_First_PositiveTestDiagnosis + 60
                ),
            OutcomeMax := paste0(OutcomeMax, "D")]
  
  # Hospitalized within 1 month after first pos test
  PCRcohort[COV19_positiveYN == 1 &
              data.table::between(
                x     = DaysSinceBirth_First_Hospital_Admission,
                lower = DaysSinceBirth_First_PositiveTestDiagnosis - 14,
                upper = DaysSinceBirth_First_PositiveTestDiagnosis + 30
              ),
            OutcomeMax := paste0(OutcomeMax, "H")]
  
  # ICU stay within 1 month after first pos test
  PCRcohort[COV19_positiveYN == 1 &
              data.table::between(
                x     = DaysSinceBirth_First_ICU_Admission,
                lower = DaysSinceBirth_First_PositiveTestDiagnosis - 14,
                upper = DaysSinceBirth_First_PositiveTestDiagnosis + 30
              ),
            OutcomeMax := paste0(OutcomeMax,"I")]
  
  # Never discharged since testing positive
  PCRcohort[COV19_positiveYN == 1 & grepl("H|S",OutcomeMax) &
              is.na(DaysSinceBirth_Last_Hospital_Discharge),
            OutcomeMax := paste0(OutcomeMax, "S")]
  
  # Create non-overlapping outcomes and factorize
  PCRcohort[, OutcomeMax := factor(OutcomeMax,
                                   levels = c("N", "P", "PD", "PH", "PHS", "PDH", "PHI", "PHIS", "PDHI"),
                                   labels = c(
                                     "Tested Negative",
                                     "Not hospitalized",
                                     "Not hospitalized / Deceased",
                                     "Hospitalized / Discharged",
                                     "Still hospitalized",
                                     "Hospitalized / Deceased",
                                     "ICU / Discharged",
                                     "ICU / Still Hospitalized",
                                     "ICU / Deceased")
  )]
  
  setnames(PCRcohort, c("Encrypted_PatientID", "BMI_class"), c("id", "BMIcategory"))
  
  PCRcohort <- PCRcohort[, dsb_earliest_test_dx_date := pmin(DaysSinceBirth_EARLIEST_TEST_DATE, DaysSinceBirth_EARLIEST_DX_DATE, na.rm = TRUE)][]
  
  PCRcohort <- PCRcohort[, in_phenome := fifelse(id %in% tested_phenome_ids, 1, 0)][
    , AnyCancerPhe := fcase(
      in_phenome == 1 & id %in% cancer_tested_ids, 1,
      in_phenome == 1 & id %notin% cancer_tested_ids, 0
    )
  ]
  
  PCRcohort <- PCRcohort[, `:=` (
    `Test Results` = fifelse(OutcomeMax == "Tested Negative", 0, 1),
    Hospitalized   = fifelse(OutcomeMax %in% outvar[["hospitalized"]], 1, 0),
    ICU            = fifelse(OutcomeMax %in% outvar[["icu_admission"]], 1, 0),
    Deceased       = fifelse(OutcomeMax %in% outvar[["mortality"]], 1, 0),
    `Cohort Type`  = "Tested"
  )][]
  
  PCRcohort <- PCRcohort[, `Severe COVID` := fifelse(Hospitalized == 1 | ICU == 1 | Deceased == 1, 1, 0)]
  
  return(PCRcohort[])
}
