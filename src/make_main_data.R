# Loads, manipulates, and prepares clean data file for analysis
make_main_data <- function(save = TRUE, chrt = "20220701") {

  file_paths <- get_file_paths(cohort = chrt)
  
  # paths -------------
  cov_path      <- file_paths$covariates
  proc_path     <- file_paths$procedures
  comorbid_path <- file_paths$comorbidities
  
  # load phecode data ----------
  icd_phecode_tested <- load_icd_phecode_tested_data(
    icd9_path  = file_paths$icd9,
    icd10_path = file_paths$icd10
  )
  icd_phecode        <- sex_concordance_check(x = icd_phecode_tested, tested = TRUE)
  
  # load PCRcohort data -----------
  load(cov_path)
  
  # get key dates ------------
    # key_date as 14 days before first positive DSB; age at first positive DSB
    PCRcohort[COV19_positiveYN == 1, `:=` (
      key_date = DaysSinceBirth_First_PositiveTestDiagnosis - 14,
      key_age  = DaysSinceBirth_First_PositiveTestDiagnosis/365.25
    )]
    # key_date as 14 days before first test DSB; age at first test DSB
    PCRcohort[COV19_positiveYN == 0, `:=` (
      key_date = DaysSinceBirth_First_Test_Diagnosis - 14,
      key_age  = DaysSinceBirth_First_Test_Diagnosis/365.25
    )]
    # keep finite key_dates; restrict to >18
    PCRcohort <- PCRcohort[is.finite(key_date)]
    
    icd_phecode <- merge.data.table(
      icd_phecode,
      PCRcohort[, .(id = Encrypted_PatientID, key_date, key_age)],
      all.x = TRUE, all.y = FALSE,
      by = "id"
    )
    
    icd_phecode[, three_years := ((key_date + 14) - (365.25*3))]
    
    # restrict phenome to observations prior to key date by individual
    sub_phecode <- icd_phecode[!is.na(key_date) & DaysSinceBirth < key_date]
    
    # restrict phenome to people 18+ at time of index test
    sub_phecode <- sub_phecode[key_age > 18]
    
    # select first occurrence of each phecode by person
    setkeyv(sub_phecode, cols = c("id", "phecode"))
    sub_phecode <- sub_phecode[sub_phecode[, .I[DaysSinceBirth == min(DaysSinceBirth)], by = key(sub_phecode)]$V1]
  
  # get ids -----------
    source("~/projects/covid/new_cancer/lists/cancer_codes.R")
  
  cancer <- sub_phecode[phecode %in% codes$cancer_phecodes]
  
  skin_cancer_ids       <- get_ids(x = sub_phecode, filter_codes = codes$skin_cancer_phecodes)
  heme_malign_ids       <- get_ids(x = sub_phecode, filter_codes = unique(unlist(codes$heme_malign)))
  myeloid_ids           <- get_ids(x = sub_phecode, filter_codes = codes$heme_malign$myeloid)
  lymphoid_ids          <- get_ids(x = sub_phecode, filter_codes = codes$heme_malign$lymphoid)
  breast_cancer_ids     <- get_ids(x = sub_phecode, filter_codes = codes$breast_cancer_phecodes)
  bladder_cancer_ids    <- get_ids(x = sub_phecode, filter_codes = codes$bladder_cancer_phecodes)
  kidney_cancer_ids     <- get_ids(x = sub_phecode, filter_codes = codes$kidney_cancer_phecodes)
  prostate_cancer_ids   <- get_ids(x = sub_phecode, filter_codes = codes$prostate_cancer_phecodes)
  colorectal_cancer_ids <- get_ids(x = sub_phecode, filter_codes = codes$colorectal_cancer_phecodes)
  lung_cancer_ids       <- get_ids(x = sub_phecode, filter_codes = codes$lung_cancer_phecodes)
  other_cancer_ids      <- get_ids(x = sub_phecode, filter_codes = codes$cancer_phecodes[!(codes$cancer_phecodes %in% c(
    codes$skin_cancer_phecodes,
    codes$colorectal_cancer_phecodes,
    codes$bladder_cancer_phecodes,
    codes$kidney_cancer_phecodes,
    codes$heme_malign_phecodes,
    codes$breast_cancer_phecodes,
    codes$prostate_cancer_phecodes,
    codes$lung_cancer_phecodes,
    unique(unlist(codes$heme_malign))
  ))])
  
  # process covariate data -----------
  outcomes <- readRDS("~/projects/covid/new_cancer/lists/outcomes.rds")
  
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

    # assign phenome indicator, an any cancer indicator, and outcome indicators
    PCRcohort[
      , in_phenome := ifelse(id %in% sub_phecode[, unique(id)], 1, 0)
    ][, `:=` (
      AnyCancerPhe = fcase(
        in_phenome == 1 & id %in% cancer[tested == 1, unique(id)], 1,
        in_phenome == 1 & id %notin% cancer[tested == 1, unique(id)], 0
      ),
      `Test Results` = fifelse(OutcomeMax == "Tested Negative", 0, 1),
      Hospitalized   = fifelse(OutcomeMax %in% outcomes[["hospitalized"]], 1, 0),
      ICU            = fifelse(OutcomeMax %in% outcomes[["icu_admission"]], 1, 0),
      Deceased       = fifelse(OutcomeMax %in% outcomes[["mortality"]], 1, 0),
      `Cohort Type`  = "Tested"
    )]
    
    PCRcohort[, `Severe COVID` := fifelse(Hospitalized == 1 | ICU == 1 | Deceased == 1, 1, 0)]
  
  tested_cov_data <- PCRcohort[in_phenome == 1][!(`Test Results` == 1 & is.na(key_date))]
  
  # deal with comorbidities ----------
  comorbidities <- readRDS(comorbid_path)
  
  respiratory_ids <- sub_phecode[phecode %in% comorbidities$respiratory][, unique(id)]
  circulatory_ids <- sub_phecode[phecode %in% comorbidities$circulatory][, unique(id)]
  diabetes_ids    <- sub_phecode[phecode %in% comorbidities$diabetes][, unique(id)]
  kidney_ids      <- sub_phecode[phecode %in% comorbidities$kidney][, unique(id)]
  liver_ids       <- sub_phecode[phecode %in% comorbidities$liver][, unique(id)]
  autoimmune_ids  <- sub_phecode[phecode %in% comorbidities$autoimmune][, unique(id)]
  
  old_comorbids <- c("RespiratoryDiseases", "CirculatoryDiseases", "Type2Diabetes", "KidneyDiseases", "LiverDiseases", "AutoimmuneDiseases")
  
  setnames(tested_cov_data, old = c(old_comorbids, "ComorbidityScore"), new = paste0(c(old_comorbids, "ComorbidityScore"), "_old"))
  
  tested_cov_data[, RespiratoryDiseases := ifelse(id %in% respiratory_ids, 1, 0)]
  tested_cov_data[, CirculatoryDiseases := ifelse(id %in% circulatory_ids, 1, 0)]
  tested_cov_data[, Type2Diabetes := ifelse(id %in% diabetes_ids, 1, 0)]
  tested_cov_data[, KidneyDiseases := ifelse(id %in% kidney_ids, 1, 0)]
  tested_cov_data[, LiverDiseases := ifelse(id %in% liver_ids, 1, 0)]
  tested_cov_data[, AutoimmuneDiseases := ifelse(id %in% autoimmune_ids, 1, 0)]
  
  tested_cov_data[, ComorbidityScore := rowSums(.SD, na.rm = TRUE), .SDcols = old_comorbids]
  
  # cancer treatment data -----------
  cli::cli_alert_info("loading cancer treatment data")
  
  # unmatched_to_exclude <- c("")
  
  procedures <- fread(proc_path)[, !c("MGI_DeID_PatientId")]
  setnames(procedures, c("Encrypted_PatientID"), c("id"))
  
  procedures <- merge.data.table(
    procedures,
    tested_cov_data[, .(id, key_date)],
    all.x = TRUE, all.y = FALSE,
    by = "id"
  )
  
  procedures <- procedures[id %in% tested_cov_data[, id]][ProcedureDate_DaysSinceBirth < key_date]
  
  chemo_icd <- sub_phecode[DiagnosisCode %in% codes$chemo$chemo_icd_codes]
  rad_icd   <- sub_phecode[DiagnosisCode %in% codes$radiation$radiation_icd_codes]
  imt_icd   <- sub_phecode[DiagnosisCode %in% codes$immunotherapy$immunotherapy_icd_codes]
  
  chemo_cpt <- code_filter(x = procedures, codes = codes$chemo$chemo_cpt_codes)
  rad_cpt   <- code_filter(x = procedures, codes = codes$radiation$radiation_cpt_codes)
  surgery   <- code_filter(x = procedures, codes = c(codes$surgery$surgery_icd_codes, codes$surgery$surgery_cpt_codes))
  imt_cpt   <- code_filter(x = procedures, codes = codes$immunotherapy$immunotherapy_icd_codes)
  
  # unspecified codes ----------
  unspec_chemo <- merge.data.table(
    code_filter(x = procedures, codes = codes$chemo$chemo_unspec_codes),
    cancer,
    by.x = c("id", "ProcedureDate_DaysSinceBirth"),
    by.y = c("id", "DaysSinceBirth"),
    all = FALSE)
  
  unspec_radiation <- merge.data.table(
    code_filter(x = procedures, codes = codes$radiation$radiation_unspec_codes),
    cancer,
    by.x = c("id", "ProcedureDate_DaysSinceBirth"),
    by.y = c("id", "DaysSinceBirth"),
    all = FALSE)
  
  surgery_same_dsb <- merge.data.table(
    surgery,
    cancer,
    by.x = c("id", "ProcedureDate_DaysSinceBirth"),
    by.y = c("id", "DaysSinceBirth"),
    all = FALSE)

  # combining data -----------
  cli::cli_alert_info("combining data")
  
  num_vars <- c("Age", "BMI",
                "disadvantage2_13_17_qrtl",
                "popden13_17_qrtl", "RespiratoryDiseases",
                "CirculatoryDiseases", "AnyCancer", "Type2Diabetes",
                "KidneyDiseases", "LiverDiseases",
                "AutoimmuneDiseases")
  factor_vars <- c("Drinker", "AnyCancerPhe", "AnyCancer",
                   "RespiratoryDiseases", "CirculatoryDiseases",
                   "Type2Diabetes", "KidneyDiseases",
                   "LiverDiseases", "AutoimmuneDiseases")
  
  # turn numeric variables into numeric variables
  combined <- tested_cov_data[in_phenome == 1][, Outcome := OutcomeMax][, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars]
  
  # remake the top Age Category
  combined <- combined[AgeCategory %in% c("[80,100)"), AgeCategory := "[80+)"][, AgeCategory := droplevels(AgeCategory)]
  
  combined <- combined[, ComborbidityScore.old := as.numeric(ComorbidityScore)][
    , ComorbidityScore := RespiratoryDiseases + CirculatoryDiseases + Type2Diabetes + KidneyDiseases + LiverDiseases + AutoimmuneDiseases
  ][
    , c("Hospitalized", "ICU", "Deceased") := lapply(.SD, as.numeric), .SDcols = c("Hospitalized", "ICU", "Deceased")][
      , `:=` (
        Age.old = Age,
        AgeCategory = relevel(
          forcats::fct_collapse(
            as.factor(AgeCategory),
            `[65+)` = c("[65,80)", "[80+)")
          ), 
          ref = "[18,35)"),
        BMIcategory    = relevel(as.factor(BMIcategory), ref = "[18.5,25)"),
        RaceEthnicity4 = relevel(as.factor(RaceEthnicity4), ref = "Caucasian / Non-Hispanic"),
        SmokingStatus  = relevel(as.factor(SmokingStatus), ref = "Never")
      )
    ][
      , (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars
    ][
      , `:=` (
        Age = Age/10
      )
    ][, `:=` (
      skin_cancer       = fifelse(id %in% skin_cancer_ids, 1, 0),
      heme_malign       = fifelse(id %in% heme_malign_ids, 1, 0),
      lymphoid          = fifelse(id %in% lymphoid_ids, 1, 0),
      myeloid           = fifelse(id %in% myeloid_ids, 1, 0),
      kidney_cancer     = fifelse(id %in% kidney_cancer_ids, 1, 0),
      bladder_cancer    = fifelse(id %in% bladder_cancer_ids, 1, 0),
      breast_cancer     = fifelse(id %in% breast_cancer_ids, 1, 0),
      prostate_cancer   = fifelse(id %in% prostate_cancer_ids, 1, 0),
      lung_cancer       = fifelse(id %in% lung_cancer_ids, 1, 0),
      other_cancer      = fifelse(id %in% other_cancer_ids, 1, 0),
      colorectal_cancer = fifelse(id %in% colorectal_cancer_ids, 1, 0)
    )][
      , `:=` (
        radiation   = fifelse(id %in% c(rad_cpt$id, rad_icd$id, unspec_radiation$id), 1, 0),
        surgery     = fifelse(id %in% c(surgery$id), 1, 0),
        surgery_dsb = fifelse(id %in% surgery_same_dsb$id, 1, 0),
        chemo       = fifelse(id %in% c(chemo_cpt$id, chemo_icd$id, unspec_chemo$id), 1, 0),
        imt         = fifelse(id %in% c(imt_cpt$id, imt_icd), 1, 0)
      )
    ][
      , `:=` (
        rad_only     = fifelse(radiation == 1 & chemo == 0 & surgery_dsb == 0, 1, 0),
        surgery_only = fifelse(surgery_dsb == 1 & chemo == 0 & radiation == 0, 1, 0)
      )
    ][
      Age >= 0
    ][]
  
  # get last initial cancer diagnosis ---------
  setkeyv(sub_phecode, cols = c("id"))
  
  last_cancer <- sub_phecode[phecode %in% codes$cancer_phecodes][order(-DaysSinceBirth), head(.SD, 1), by = key(sub_phecode)]
  
  last_cancer[, years_since_cancer := ((key_date + 14) - DaysSinceBirth) / 365.25]
  last_cancer[, yscancer_category := fcase(
    years_since_cancer >= 0 & years_since_cancer < 3, "[0,3)",
    years_since_cancer >= 3 & years_since_cancer < 10, "[3,10)",
    years_since_cancer >= 10, "[10+)"
  )]
  
  combined <- merge.data.table(
    combined,
    last_cancer[, .(id, years_since_cancer, yscancer_category)],
    all.x = TRUE, all.y = FALSE,
    by = "id"
  )
  
  # last chemo procedure ----------
  all_chemo <- unique(rbindlist(list(
    chemo_icd[, .(id, dsb = DaysSinceBirth, key_date)],
    chemo_cpt[, .(id, dsb = ProcedureDate_DaysSinceBirth, key_date)],
    unspec_chemo[, .(id, dsb = ProcedureDate_DaysSinceBirth, key_date = key_date.x)]
  )))
  
  last_chemo <- all_chemo[order(-dsb), head(.SD, 1), by = "id"]
  
  last_chemo[, years_since_chemo := ((key_date + 14) - dsb) /365.25]
  last_chemo[, yschemo_category := fcase(
    years_since_chemo >= 0 & years_since_chemo < 1, "[0,1)",
    years_since_chemo >= 1 & years_since_chemo < 3, "[1,3)",
    years_since_chemo >= 3, "[3+)"
  )]
  
  combined <- merge.data.table(
    combined,
    last_chemo[, .(id, years_since_chemo, yschemo_category)],
    all.x = TRUE, all.y = FALSE,
    by = "id"
  )
  
  
  # add none categories and factorize -----------
  combined[is.na(yscancer_category), yscancer_category := "No cancer"]
  combined[is.na(yschemo_category), yschemo_category := "No chemotherapy"]
  combined[AnyCancerPhe == 0, yschemo_category := "No chemotherapy"]
  
  combined[, `:=` (
    yscancer_category = relevel(factor(yscancer_category), ref = "No cancer"),
    yschemo_category = relevel(factor(yschemo_category), ref = "No chemotherapy")
  )]
  
  combined[, chemo_strat_treatment := yschemo_category]
  combined[chemo_strat_treatment == "No chemotherapy", chemo_strat_treatment := fifelse(
    rad_only == 1 & AnyCancerPhe == 1,"Radiation Only", fifelse(
      surgery_only == 1 & AnyCancerPhe == 1, "Surgery Only", fifelse(
        AnyCancerPhe == 1, "No treatment/Unknown", "No cancer"
      )
    )
  )]
  combined[, chemo_strat_treatment := droplevels(relevel(factor(chemo_strat_treatment), ref = "No cancer"))]
  
  # add vaccination variable
  combined <- get_pre_positive_vax_status(x = combined)[in_phenome == 1]
  
  # add cancer types and treatment variables
  # create cancer_treatment variable ----------
  combined <- combined[, cancer_treatment := fifelse(
    chemo == 1 & AnyCancerPhe == 1, "Chemotherapy", fifelse(
      rad_only == 1 & AnyCancerPhe == 1, "Radiation Only", fifelse(
        surgery_only == 1 & AnyCancerPhe == 1, "Surgery Only", fifelse(
          AnyCancerPhe == 1, "No treatment/Unknown", "No cancer"
        )
      )
    )
  )][!is.na(AnyCancerPhe)][]
  
  # create cancer type variable ----------
  # combined[skin_cancer == 1, cancer_type := "Melanoma"]
  # combined[heme_malign == 1, cancer_type := "Hematologic malignancy"]
  # combined[breast_cancer == 1, cancer_type := "Breast cancer"]
  # combined[prostate_cancer == 1, cancer_type := "Prostate cancer"]
  # combined[lung_cancer == 1, cancer_type := "Lung cancer"]
  # combined[is.na(cancer_type) & AnyCancerPhe == 1, cancer_type := "Other cancer"]
  # combined[is.na(cancer_type) & AnyCancerPhe == 0, cancer_type := "No cancer"]
  
  # factorize variables and set reference groups -------------
  combined[, cancer_treatment := relevel(factor(cancer_treatment), ref = "No cancer")]
  # combined[, cancer_type := relevel(factor(cancer_type), ref = "No cancer")]
  combined[, vax_status := relevel(factor(vax_status), ref = "Before vaccination")]
  
  # combine categories and update vax definition
  combined[RaceEthnicity4 %in% c("Other / Non-Hispanic or Hispanic", "Other / Unknown Ethnicity"), RaceEthnicity4 := "Other/Unknown"][, RaceEthnicity4 := relevel(factor(RaceEthnicity4), ref = "Caucasian / Non-Hispanic")]
  
  # add recent cancer variables -----------
  combined[, key_three := ((key_date + 14) - (365.25*3))]
  
  rec_cancer <- cancer[DaysSinceBirth > three_years]
  
  recent_skin_cancer_ids       <- rec_cancer[phecode %in% codes$skin_cancer_phecodes, unique(id)]
  recent_heme_malign_ids       <- rec_cancer[phecode %in% unlist(codes$heme_malign), unique(id)]
  recent_lymphoid_ids          <- rec_cancer[phecode %in% codes$heme_malign$lymphoid, unique(id)]
  recent_myeloid_ids           <- rec_cancer[phecode %in% codes$heme_malign$myeloid, unique(id)]
  recent_breast_cancer_ids     <- rec_cancer[phecode %in% codes$breast_cancer_phecodes, unique(id)]
  recent_bladder_cancer_ids    <- rec_cancer[phecode %in% codes$bladder_cancer_phecodes, unique(id)]
  recent_kidney_cancer_ids     <- rec_cancer[phecode %in% codes$kidney_cancer_phecodes, unique(id)]
  recent_colorectal_cancer_ids <- rec_cancer[phecode %in% codes$colorectal_cancer_phecodes, unique(id)]
  recent_prostate_cancer_ids   <- rec_cancer[phecode %in% codes$prostate_cancer_phecodes, unique(id)]
  recent_lung_cancer_ids       <- rec_cancer[phecode %in% codes$lung_cancer_phecodes, unique(id)]
  recent_other_cancer_ids      <- rec_cancer[phecode %in% codes$cancer_phecodes[!(codes$cancer_phecodes %in% c(
    codes$skin_cancer_phecodes,
    codes$colorectal_cancer_phecodes,
    codes$bladder_cancer_phecodes,
    codes$kidney_cancer_phecodes,
    codes$heme_malign_phecodes,
    codes$breast_cancer_phecodes,
    codes$prostate_cancer_phecodes,
    codes$lung_cancer_phecodes,
    unique(unlist(codes$heme_malign))
  ))], unique(id)]
  
  ###
  
  combined[, `:=` (
    recent_AnyCancerPhe      = fifelse(id %in% rec_cancer[, unique(id)], 1, 0),
    recent_skin_cancer       = fifelse(id %in% recent_skin_cancer_ids, 1, 0),
    recent_lymphoid          = fifelse(id %in% recent_lymphoid_ids, 1, 0),
    recent_myeloid           = fifelse(id %in% recent_myeloid_ids, 1, 0),
    recent_bladder_cancer    = fifelse(id %in% recent_bladder_cancer_ids, 1, 0),
    recent_kidney_cancer     = fifelse(id %in% recent_kidney_cancer_ids, 1, 0),
    recent_colorectal_cancer = fifelse(id %in% recent_colorectal_cancer_ids, 1, 0),
    recent_heme_malign       = fifelse(id %in% recent_heme_malign_ids, 1, 0),
    recent_breast_cancer     = fifelse(id %in% recent_breast_cancer_ids, 1, 0),
    recent_prostate_cancer   = fifelse(id %in% recent_prostate_cancer_ids, 1, 0),
    recent_lung_cancer       = fifelse(id %in% recent_lung_cancer_ids, 1, 0),
    recent_other_cancer      = fifelse(id %in% recent_other_cancer_ids, 1, 0)
  )]
  
  # combined[, recent_other_cancer := 0]
  # combined[recent_skin_cancer == 0 & recent_heme_malign == 0 & recent_breast_cancer == 0 & recent_prostate_cancer == 0 & recent_lung_cancer == 0 & AnyCancerPhe == 1, other_cancer := 1]
  # 
  # recent cancer treatment variables -----------
  rec_phecode    <- sub_phecode[DaysSinceBirth > three_years]
  rec_procedures <- procedures[, three_years := ((key_date + 14) - (365.25*3))][ProcedureDate_DaysSinceBirth > three_years]
  
  recent_chemo_icd <- unique(rec_phecode[DiagnosisCode %in% codes$chemo$chemo_icd_codes][, id])
  recent_rad_icd   <- unique(rec_phecode[DiagnosisCode %in% codes$radiation$radiation_icd_codes][, id])
  recent_imt_icd   <- unique(rec_phecode[DiagnosisCode %in% codes$immunotherapy$immunotherapy_icd_codes][, id])
  
  recent_chemo_cpt <- unique(code_filter(x = rec_procedures, codes = codes$chemo$chemo_cpt_codes)[, id])
  recent_rad_cpt   <- unique(code_filter(x = rec_procedures, codes = codes$radiation$radiation_cpt_codes)[, id])
  recent_surgery   <- code_filter(x = rec_procedures, codes = c(codes$surgery$surgery_icd_codes, codes$surgery$surgery_cpt_codes))
  recent_imt_cpt   <- unique(code_filter(x = rec_procedures, codes = codes$immunotherapy$immunotherapy_icd_codes)[, id])
  
  recent_unspec_chemo <- merge.data.table(
    code_filter(x = rec_procedures, codes = codes$chemo$chemo_unspec_codes),
    rec_cancer,
    by.x = c("id", "ProcedureDate_DaysSinceBirth"),
    by.y = c("id", "DaysSinceBirth"),
    all = FALSE)[, unique(id)]
  
  recent_unspec_radiation <- merge.data.table(
    code_filter(x = rec_procedures, codes = codes$radiation$radiation_unspec_codes),
    rec_cancer,
    by.x = c("id", "ProcedureDate_DaysSinceBirth"),
    by.y = c("id", "DaysSinceBirth"),
    all = FALSE)[, unique(id)]
  
  recent_surgery_same_dsb <- merge.data.table(
    recent_surgery,
    rec_cancer,
    by.x = c("id", "ProcedureDate_DaysSinceBirth"),
    by.y = c("id", "DaysSinceBirth"),
    all = FALSE)[, unique(id)]
  
  cli::cli_alert_info("constructing recent cancer variables...")
  # restricted <- make_restricted_phenome_data(input = combined)
  
  recent_surgery_ids <- unique(recent_surgery[, id])
  
  combined[
    , `:=` (
      recent_radiation   = fifelse(id %in% c(recent_rad_cpt, recent_rad_icd, recent_unspec_radiation), 1, 0),
      recent_surgery     = fifelse(id %in% recent_surgery_ids, 1, 0),
      recent_surgery_dsb = fifelse(id %in% recent_surgery_same_dsb, 1, 0),
      recent_chemo       = fifelse(id %in% c(recent_chemo_cpt, recent_chemo_icd, recent_unspec_chemo), 1, 0),
      recent_imt         = fifelse(id %in% c(recent_imt_cpt, recent_imt_icd), 1, 0)
    )
  ][
    , `:=` (
      recent_rad_only     = fifelse(recent_radiation == 1 & recent_chemo == 0 & recent_surgery_dsb == 0, 1, 0),
      recent_surgery_only = fifelse(recent_surgery_dsb == 1 & recent_chemo == 0 & recent_radiation == 0, 1, 0)
    )
  ]

  combined <- combined[, recent_cancer_treatment := fifelse(
    recent_chemo == 1 & recent_AnyCancerPhe == 1, "Chemotherapy", fifelse(
      recent_rad_only == 1 & recent_AnyCancerPhe == 1, "Radiation Only", fifelse(
        recent_surgery_only == 1 & recent_AnyCancerPhe == 1, "Surgery Only", fifelse(
          recent_AnyCancerPhe == 1, "No treatment/Unknown", "No cancer"
        )
      )
    )
  )][!is.na(AnyCancerPhe)][]
  
  # create cancer type variable ----------
  # combined[recent_skin_cancer == 1, recent_cancer_type := "Melanoma"]
  # # combined[recent_heme_malign == 1, recent_cancer_type := "Hematologic malignancy"]
  # combined[recent_lymphoid == 1, recent_cancer_type := "Lymphoid"]
  # combined[recent_myeloid == 1, recent_cancer_type := "Myeloid"]
  # combined[recent_bladder_cancer == 1, recent_cancer_type := "Bladder cancer"]
  # combined[recent_kidney_cancer == 1, recent_cancer_type := "Kidney cancer"]
  # combined[recent_colorectal_cancer == 1, recent_cancer_type := "Colorectal cancer"]
  # combined[recent_breast_cancer == 1, recent_cancer_type := "Breast cancer"]
  # combined[recent_prostate_cancer == 1, recent_cancer_type := "Prostate cancer"]
  # combined[recent_lung_cancer == 1, recent_cancer_type := "Lung cancer"]
  # combined[is.na(recent_cancer_type) & AnyCancerPhe == 1, recent_cancer_type := "Other cancer"]
  # combined[is.na(recent_cancer_type) & AnyCancerPhe == 0, recent_cancer_type := "No cancer"]
  
  # factorize variables and set reference groups -------------
  combined[, recent_cancer_treatment := relevel(factor(recent_cancer_treatment), ref = "No cancer")]
  # combined[, recent_cancer_type := relevel(factor(recent_cancer_type), ref = "No cancer")]
  
  # create indicator variable for COVID-19 cases diagnosed in 2020 ------------
  i2020_ids <- get_2020indicator_ids(x = combined)
  i2021_ids <- get_2021indicator_ids(x = combined)
  combined[, i2020 := fifelse(id %in% i2020_ids, "2020", fifelse(id %in% i2021_ids, "2021", "2022"))]
  combined[, i2020 := relevel(factor(i2020), ref = "2022")]
  # combined[, i2020 := fifelse(id %in% i2020_ids, 1, 0)]
  
  # remove observation with Age = 0 ----------
  combined <- combined[Age != 0]
  combined[, AgeCategory := droplevels(AgeCategory)]
  
  # chemo for cancer ------------
  combined[AnyCancerPhe == 0, chemo := 0]
  
  # stratify recent cancer type variables -----------
  combined[, recent_skin_cancer := fifelse(recent_skin_cancer == 1, "[0, 3)", fifelse(skin_cancer == 1, "[3+)", "No skin cancer"))]
  combined[, recent_skin_cancer := relevel(factor(recent_skin_cancer), ref = "No skin cancer")]
  
  combined[, recent_lymphoid := fifelse(recent_lymphoid == 1, "[0, 3)", fifelse(lymphoid == 1, "[3+)", "No lymphoid"))]
  combined[, recent_lymphoid := relevel(factor(recent_lymphoid), ref = "No lymphoid")]
  
  combined[, recent_myeloid := fifelse(recent_myeloid == 1, "[0, 3)", fifelse(myeloid == 1, "[3+)", "No myeloid"))]
  combined[, recent_myeloid := relevel(factor(recent_myeloid), ref = "No myeloid")]
  
  combined[, recent_bladder_cancer := fifelse(recent_bladder_cancer == 1, "[0, 3)", fifelse(bladder_cancer == 1, "[3+)", "No bladder cancer"))]
  combined[, recent_bladder_cancer := relevel(factor(recent_bladder_cancer), ref = "No bladder cancer")]
  
  combined[, recent_kidney_cancer := fifelse(recent_kidney_cancer == 1, "[0, 3)", fifelse(kidney_cancer == 1, "[3+)", "No kidney cancer"))]
  combined[, recent_kidney_cancer := relevel(factor(recent_kidney_cancer), ref = "No kidney cancer")]
  
  combined[, recent_colorectal_cancer := fifelse(recent_colorectal_cancer == 1, "[0, 3)", fifelse(colorectal_cancer == 1, "[3+)", "No colorectal cancer"))]
  combined[, recent_colorectal_cancer := relevel(factor(recent_colorectal_cancer), ref = "No colorectal cancer")]
  
  combined[, recent_breast_cancer := fifelse(recent_breast_cancer == 1, "[0, 3)", fifelse(breast_cancer == 1, "[3+)", "No breast cancer"))]
  combined[, recent_breast_cancer := relevel(factor(recent_breast_cancer), ref = "No breast cancer")]
  
  combined[, recent_prostate_cancer := fifelse(recent_prostate_cancer == 1, "[0, 3)", fifelse(prostate_cancer == 1, "[3+)", "No prostate cancer"))]
  combined[, recent_prostate_cancer := relevel(factor(recent_prostate_cancer), ref = "No prostate cancer")]
  
  combined[, recent_lung_cancer := fifelse(recent_lung_cancer == 1, "[0, 3)", fifelse(lung_cancer == 1, "[3+)", "No lung cancer"))]
  combined[, recent_lung_cancer := relevel(factor(recent_lung_cancer), ref = "No lung cancer")]
  
  combined[, recent_other_cancer := fifelse(recent_other_cancer == 1, "[0, 3)", fifelse(other_cancer == 1, "[3+)", "No other cancer"))]
  combined[, recent_other_cancer := relevel(factor(recent_other_cancer), ref = "No other cancer")]
  
  # categorical cancer type variables -----------
  combined[, skin_cancer_cat := fifelse(skin_cancer == 1, "Melanoma", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, skin_cancer_cat := relevel(factor(skin_cancer_cat), ref = "No cancer")]
  
  combined[, heme_malign_cat := fifelse(heme_malign == 1, "Hematologic malignancies", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, heme_malign_cat := relevel(factor(heme_malign_cat), ref = "No cancer")]
  
  combined[, lymphoid_cat := fifelse(lymphoid == 1, "Lymphoid", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, lymphoid_cat := relevel(factor(lymphoid_cat), ref = "No cancer")]
  
  combined[, myeloid_cat := fifelse(myeloid == 1, "Myeloid", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, myeloid_cat := relevel(factor(myeloid_cat), ref = "No cancer")]
  table(combined[, myeloid_cat])
  
  combined[, bladder_cancer_cat := fifelse(bladder_cancer == 1, "Bladder cancer", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, bladder_cancer_cat := relevel(factor(bladder_cancer_cat), ref = "No cancer")]
  table(combined[, bladder_cancer_cat])
  
  combined[, kidney_cancer_cat := fifelse(kidney_cancer == 1, "Kidney cancer", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, kidney_cancer_cat := relevel(factor(kidney_cancer_cat), ref = "No cancer")]
  table(combined[, kidney_cancer_cat])
  
  combined[, colorectal_cancer_cat := fifelse(colorectal_cancer == 1, "Colorectal cancer", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, colorectal_cancer_cat := relevel(factor(colorectal_cancer_cat), ref = "No cancer")]
  table(combined[, colorectal_cancer_cat])
  
  
  combined[, breast_cancer_cat := fifelse(breast_cancer == 1, "Breast cancer", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, breast_cancer_cat := relevel(factor(breast_cancer_cat), ref = "No cancer")]
  table(combined[, breast_cancer_cat])
  
  combined[, prostate_cancer_cat := fifelse(prostate_cancer == 1, "Prostate cancer", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, prostate_cancer_cat := relevel(factor(prostate_cancer_cat), ref = "No cancer")]
  table(combined[, prostate_cancer_cat])
  
  combined[, lung_cancer_cat := fifelse(lung_cancer == 1, "Lung cancer", fifelse(AnyCancerPhe == 1, "Other cancer", "No cancer"))]
  combined[, lung_cancer_cat := relevel(factor(lung_cancer_cat), ref = "No cancer")]
  table(combined[, lung_cancer_cat])
  
  
  # recent cancer treatment with combined variable for distant (3+) treatment -----------
  combined[, new_recent_cancer_treatment := recent_cancer_treatment]
  combined[new_recent_cancer_treatment == "No cancer" & cancer_treatment != "No cancer", new_recent_cancer_treatment := "Old treatment"]

  # -----------
  if (save == TRUE) {
    cli::cli_alert_info("saving whole_data object")
    saveRDS(object = combined, file = glue::glue("data/whole_data_{chrt}.rds"))
    cli::cli_alert_info("saving main_data object (tested positive only)")
    saveRDS(object = combined[`Test Results` == 1], file = glue::glue("data/main_data_{chrt}.rds"))
  }
  
  return(combined)
  
}
