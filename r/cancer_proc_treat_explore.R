library(data.table)

file_paths <- get_file_paths(cohort = "20220701")

# paths -------------
cov_path      <- file_paths$covariates
proc_path     <- file_paths$procedures
comorbid_path <- file_paths$comorbidities

source("~/projects/covid/new_cancer/lists/cancer_codes.R")

# load phecode data ----------
icd_phecode_tested <- load_icd_phecode_tested_data(
  icd9_path  = file_paths$icd9,
  icd10_path = file_paths$icd10
)
icd_phecode        <- sex_concordance_check(x = icd_phecode_tested, tested = TRUE)

cancer_diag <-  icd_phecode[, .(id, icd_code = DiagnosisCode, dsb = DaysSinceBirth, diag_lex = lexicon, tested)][icd_code %in% codes$cancer_phecodes]

# load procedure data ----------
procedures <- fread(proc_path)[, !c("MGI_DeID_PatientId")]

proc_treat <- procedures[, .(id = Encrypted_PatientID, name = ProcedureName, lex = Lexicon, dsb = ProcedureDate_DaysSinceBirth, code = ProcedureCode)][code %in% c(
  unique(unlist(codes$chemo)),
  codes$immunotherapy$immunotherapy_icd_codes,
  unique(unlist(codes$surgery)),
  unique(unlist(codes$radiation)))]

# load meds data ----------
meds_path <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883/20220701/MedAdministrations_2022-07-02.txt"

meds <- fread(meds_path)
clean_meds <- meds[!MedicationStatus %in% 
       c("MAR Hold", "Held by Provider", "Paused", "Stopped",
         "Completed", "Missed", "See Alternative", "Canceled Entry",
         "Not Given","Refused"), ][, .(id = Encrypted_PatientID, med_guid = MedicationGUID, med_name = MedicationName, dsb = DoseStartTime_DaysSinceBirth)]

# combine
tmp <- merge.data.table(
  cancer_diag,
  proc_treat,
  by = c("id", "dsb"),
  all.x = TRUE)
tmp <- merge.data.table(
  tmp,
  clean_meds,
  by = c("id", "dsb"),
  all.x = TRUE
)

na.omit(tmp, c("code", "med_guid"))
