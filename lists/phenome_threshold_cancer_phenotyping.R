# libraries ----------
library(data.table)

# file paths ----------
cov_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220202/MichiganMedicine_PCRcohort_plus_VaccinationData_20220202.Rsav"
icd9_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220202/phenomes/UNFILTERED_20220202/UNFILTERED_20220202_ICD9_Phecodes_Birthyears.Rsav"
icd10_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220202/phenomes/UNFILTERED_20220202/UNFILTERED_20220202_ICD10_Phecodes_Birthyears.Rsav"
cancer_codes_path <- "/net/wonderland/home/mmsalva/projects/covid/new_cancer/lists/cancer_codes.rds"

# PART 1: get key dates/age ----------
load(cov_path)

# key_date as 14 days before first positive DSB; age at first positive DSB
PCRcohort[COV19_positiveYN == 1, `:=` (
  key_date = DaysSinceBirth_First_PositiveTestDiagnosis - 14,
  key_age = DaysSinceBirth_First_PositiveTestDiagnosis/365.25
  )]
# key_date as 14 days before first test DSB; age at first test DSB
PCRcohort[COV19_positiveYN == 0, `:=` (
  key_date = DaysSinceBirth_First_Test_Diagnosis - 14,
  key_age = DaysSinceBirth_First_Test_Diagnosis/365.25
  )]
# keep finite key_dates; restrict to >18
PCRcohort <- PCRcohort[is.finite(key_date)][key_age > 18]

## PART 2: get phenome ----------
  # ICD 9 codes in tested/dx cohort
  load(icd9_path)
  diagnoses_ICD9 <- unique(diagnoses_ICD9)[, lexicon := "ICD9"]
  
  # ICD 10 codes in tested/dx cohort
  load(icd10_path)
  diagnoses_ICD10 <- unique(diagnoses_ICD10)[, lexicon := "ICD10"]
  
  stacked <- rbindlist(list(
    diagnoses_ICD9,
    diagnoses_ICD10
  ))[, tested := 1][]
  
  setnames(stacked, "IID", "id")

### PART 3: filter phenome ---------
  
# add key date to phenome
stacked <- merge.data.table(
  stacked,
  PCRcohort[, .(id = Encrypted_PatientID, key_date)]
)

# threshold phenome to <14 days prior to key date
sub_stacked <- stacked[DaysSinceBirth < key_date]

# number in restricted phenome
sub_stacked[, length(unique(id))]

# table of COVID19 status among adults (>18) in restricted phenome
PCRcohort[Encrypted_PatientID %in% sub_stacked[, unique(id)]][, table(COV19_positiveYN)]

# create cancer variable
cancer_codes <- readRDS(cancer_codes_path)
cancer_ids   <- sub_stacked[phecode %in% cancer_codes$cancer_phecodes, unique(id)]

PCRcohort[, Cancer := ifelse(Encrypted_PatientID %in% cancer_ids, 1, 0)]

# table of COVID and cancer
table(Cancer = PCRcohort[, Cancer], COVID = PCRcohort[, COV19_positiveYN])

# analytic ids (assuming no missing covariates)
key_ids <- PCRcohort[, Encrypted_PatientID]
