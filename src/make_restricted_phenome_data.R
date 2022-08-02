make_restricted_phenome_data <- function(input) {
  
  outcomes <- readRDS("~/projects/covid/new_cancer/lists/outcomes.rds")
  
  # covid positive ids -------------
  covid_positive_ids <- input[`Test Results` == 1 & in_phenome == 1][, unique(id)]
  
  # DSB-stamped data -------------
  cli::cli_alert_info("reading in dsb-stamped data...")
  
  threshold <- get_threshold_date()
  
  tested_phenome          <- load_icd_phecode_tested_data()
  tested_positive_phenome <- tested_phenome[id %in% covid_positive_ids]
  
  setkeyv(tested_positive_phenome, c("id", "phecode"))
  tested_positive_phenome <- tested_positive_phenome[order(DaysSinceBirth)][DaysSinceBirth >= 0]
  tested_positive_phenome <- tested_positive_phenome[tested_positive_phenome[order(DaysSinceBirth), .I[which.min(DaysSinceBirth)], by = c("id", "phecode")][, V1]]
  
  tested_positive_phenome  <- sex_concordance_check(x = tested_positive_phenome, tested = TRUE)
  tested_positive_phenome  <- tested_positive_phenome[, !c("lexicon", "tested")]
  
  tested_positive_phenome <- merge.data.table(
    tested_positive_phenome,
    threshold,
    all.x = TRUE, all.y = FALSE,
    by = "id"
  )
  
  tested_positive_phenome <- rbindlist(list(
    tested_positive_phenome[DaysSinceBirth < threshold_dsb],
    tested_positive_phenome[is.na(threshold_dsb)]
  ))
  
  # tested covariate data -------------
  cli::cli_alert_info("reading in covariate data...")
  tested_cov_data    <- load_tested_covariate_data(
    outvar             = outcomes,
    tested_phenome_ids = tested_positive_phenome[, unique(id)],
    cancer_tested_ids  = ""
  )[id %in% tested_positive_phenome[, unique(id)]]
  
  first_dx_dsb <- tested_cov_data[, .(id, first_dx_dsb = DaysSinceBirth_First_PositiveTestDiagnosis)][, three_years := first_dx_dsb - (3*365)][]
  
  # merge in dx dsb ----------
  setkeyv(tested_positive_phenome, "id")
  setkeyv(first_dx_dsb, "id")
  
  merged <- merge.data.table(
    tested_positive_phenome,
    first_dx_dsb
  )
  
  cli::cli_alert_info("restricting phenome to 3 years prior to diagnosis...")
  restricted <- merged[DaysSinceBirth <= (first_dx_dsb - 14) & DaysSinceBirth >= three_years][]
  
  # get cancer ids -----------
  codes               <- readRDS("~/projects/covid/new_cancer/lists/cancer_codes.rds")
  skin_cancer_ids     <- restricted[phecode %in% codes$skin_cancer_phecodes, unique(id)]
  heme_malign_ids     <- restricted[phecode %in% codes$heme_malign_phecodes, unique(id)]
  breast_cancer_ids   <- restricted[phecode %in% codes$breast_cancer_phecodes, unique(id)]
  prostate_cancer_ids <- restricted[phecode %in% codes$prostate_cancer_phecodes, unique(id)]
  lung_cancer_ids     <- restricted[phecode %in% codes$lung_cancer_phecodes, unique(id)]
  
  cancer              <- restricted[phecode %in% codes$cancer_phecodes]
  cancer_ids          <- restricted[phecode %in% codes$cancer_phecodes, unique(id)]
  
  # cancer treatment data ----------
  cli::cli_alert_info("loading cancer treatment data")
  procedures <- load_procedure_data(exclude_ids = "")
  procedures <- procedures[id %in% restricted[, unique(id)]]
  
  procedures <- merge.data.table(
    procedures,
    threshold,
    all.x = TRUE, all.y = FALSE,
    by = "id"
  )
  
  procedures <- rbindlist(list(
    procedures[ProcedureDate_DaysSinceBirth < threshold_dsb],
    procedures[is.na(threshold_dsb)]
  ))
  
  chemo_icd <- restricted[DiagnosisCode %in% codes$chemo$chemo_icd_codes]
  rad_icd   <- restricted[DiagnosisCode %in% codes$radiation$radiation_icd_codes]
  imt_icd   <- restricted[DiagnosisCode %in% codes$immunotherapy$immunotherapy_icd_codes]
  
  chemo_cpt <- code_filter(x = procedures, codes = codes$chemo$chemo_cpt_codes)
  rad_cpt   <- code_filter(x = procedures, codes = codes$radiation$radiation_cpt_codes)
  surgery   <- code_filter(x = procedures, codes = c(codes$surgery$surgery_icd_codes, codes$surgery$surgery_cpt_codes))
  imt_cpt   <- code_filter(x = procedures, codes = codes$immunotherapy$immunotherapy$cpt_codes)
  
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
  
  # # calculate last dates ----------
  # c19_dx <- tested_cov_data[, .(id, dsb_c19_test_or_dx = DaysSinceBirth_First_PositiveTestDiagnosis)]
  # 
  # last_radiation <- get_last_dsb(cpt_dat = rad_cpt, icd_dat = rad_icd, c19_dsb = c19_dx, treatment = "radiation")
  # last_chemo <- get_last_dsb(cpt_dat = chemo_cpt, icd_dat = chemo_icd, c19_dsb = c19_dx, treatment = "chemo")
  # 
  # last_surgery <- surgery[, .SD[ProcedureDate_DaysSinceBirth == max(ProcedureDate_DaysSinceBirth, na.rm = T)], by = "id"][, .(id, dsb_last_surgery = ProcedureDate_DaysSinceBirth)][]
  # last_surgery <- merge.data.table(
  #   last_surgery,
  #   c19_dx,
  #   by = "id"
  # )[, surgery_2years := fifelse((dsb_last_surgery - dsb_c19_test_or_dx) < -730, 1, 0)][
  #   , surgery_2years := fifelse(is.na(surgery_2years), 0, surgery_2years)
  # ][, .(id, dsb_last_surgery, surgery_2years)][]
  # 
  # last_cancer <- cancer[, .SD[DaysSinceBirth == max(DaysSinceBirth, na.rm = T)], by = "id"][, .(id, dsb_last_cancer = DaysSinceBirth)]
  # last_cancer <- merge.data.table(
  #   last_cancer,
  #   c19_dx,
  #   by = "id"
  # )[, cancer_2years := fifelse((dsb_last_cancer - dsb_c19_test_or_dx) < -730, 1, 0)][
  #   , cancer_2years := fifelse(is.na(cancer_2years), 0, cancer_2years)
  # ][, .(id, dsb_last_cancer, cancer_2years)][]
  # 
  # last_active_cancer <- cancer[DiagnosisCode %notin% codes$history_of_cancer_icd_codes][, .SD[DaysSinceBirth == max(DaysSinceBirth, na.rm = T)], by = "id"][, .(id, dsb_last_active_cancer = DaysSinceBirth)]
  # last_active_cancer <- merge.data.table(
  #   last_active_cancer,
  #   c19_dx,
  #   by = "id"
  # )[, active_cancer_2years := fifelse((dsb_last_active_cancer - dsb_c19_test_or_dx) < -730, 1, 0)][
  #   , active_cancer_2years := fifelse(is.na(active_cancer_2years), 0, active_cancer_2years)
  # ][, .(id, dsb_last_active_cancer, active_cancer_2years)][]
  # 
  # last_dates <- merge.data.table(unique(last_radiation), unique(last_chemo), all = TRUE)
  # last_dates <- merge.data.table(unique(last_dates), unique(last_surgery), all = TRUE)
  # last_dates <- merge.data.table(unique(last_dates), unique(last_cancer), all = TRUE)
  # last_dates <- merge.data.table(unique(last_dates), unique(last_active_cancer), all = TRUE)
  
  # combining data
  cli::cli_alert_info("combining data")
  
  num_vars <- c("Age", "BMI", "disadvantage2_13_17_qrtl", "popden13_17_qrtl",
                "RespiratoryDiseases",
                "CirculatoryDiseases", "AnyCancer", "Type2Diabetes",
                "KidneyDiseases", "LiverDiseases",
                "AutoimmuneDiseases")
  factor_vars <- c("Drinker", "AnyCancerPhe", "AnyCancer",
                   "RespiratoryDiseases", "CirculatoryDiseases",
                   "Type2Diabetes", "KidneyDiseases",
                   "LiverDiseases", "AutoimmuneDiseases")
  
  combined <- tested_cov_data[, `:=` (Outcome = OutcomeMax, AnyCancerPhe = fifelse(id %in% cancer_ids, 1, 0))][, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars][]
  
  combined <- combined[, ComborbidityScore.old := as.numeric(ComorbidityScore)][
    , ComorbidityScore := RespiratoryDiseases + CirculatoryDiseases + Type2Diabetes + KidneyDiseases + LiverDiseases + AutoimmuneDiseases
  ][
    , c("Hospitalized", "ICU", "Deceased", "DaysSinceBirth_First_PositiveTestDiagnosis") := lapply(.SD, as.numeric), .SDcols = c("Hospitalized", "ICU", "Deceased", "DaysSinceBirth_First_PositiveTestDiagnosis")][
      , `:=` (
        AgeCategory    = relevel(as.factor(AgeCategory), ref = "[0,18)"),
        BMIcategory    = relevel(as.factor(BMIcategory), ref = "[10,18.5)"),
        RaceEthnicity4 = relevel(as.factor(RaceEthnicity4), ref = "Caucasian / Non-Hispanic"),
        SmokingStatus  = relevel(as.factor(SmokingStatus), ref = "Never")
      )
    ][
      , `:=` (
        AgeCategory = relevel(
          forcats::fct_collapse(
            as.factor(AgeCategory),
            `[0,35)`   = c("[0,18)", "[18,35)"),
            `[65,100)` = c("[65,80)", "[80,100)")
          ),
          ref = "[0,35)"),
        BMIcategory = relevel(
          forcats::fct_collapse(
            as.factor(BMIcategory),
            `[10,25)` = c("[10,18.5)", "[18.5,25)")
          ),
          ref = "[10,25)"),
        RaceEthnicity4 = relevel(as.factor(RaceEthnicity4), ref = "Caucasian / Non-Hispanic"),
        SmokingStatus = relevel(as.factor(SmokingStatus), ref = "Never")
      )
    ][
      , (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars
    ][
      , `:=` (
        Age.old = Age
      )
    ][
      , `:=` (
        NDI = disadvantage2_13_17_qrtl,
        Age = Age/10
      )
    ][, `:=` (
      recent_AnyCancerPhe    = fifelse(id %in% cancer_ids, 1, 0),
      recent_skin_cancer     = fifelse(id %in% skin_cancer_ids, 1, 0),
      recent_heme_malign     = fifelse(id %in% heme_malign_ids, 1, 0),
      recent_breast_cancer   = fifelse(id %in% breast_cancer_ids, 1, 0),
      recent_prostate_cancer = fifelse(id %in% prostate_cancer_ids, 1, 0),
      recent_lung_cancer     = fifelse(id %in% lung_cancer_ids, 1, 0)
    )][
      , `:=` (
        radiation   = fifelse(id %in% c(rad_cpt$id, rad_icd$id, unspec_radiation$id), 1, 0),
        surgery     = fifelse(id %in% c(surgery$id), 1, 0),
        surgery_dsb = fifelse(id %in% surgery_same_dsb$id, 1, 0),
        recent_chemo       = fifelse(id %in% c(chemo_cpt$id, chemo_icd$id, unspec_chemo$id), 1, 0),
        imt         = fifelse(id %in% c(imt_cpt$id, imt_icd), 1, 0)
      )
    ][
      , `:=` (
        recent_rad_only     = fifelse(radiation == 1 & recent_chemo == 0 & surgery_dsb == 0, 1, 0),
        recent_surgery_only = fifelse(surgery_dsb == 1 & recent_chemo == 0 & radiation == 0, 1, 0)
      )
    ][
      Age >= 0
    ][]
  
  # combined <- merge.data.table(
  #   combined,
  #   last_dates,
  #   by = "id",
  #   all = TRUE
  # )
  
  # add cancer types and treatment variables
  # create recent_cancer_treatment variable ----------
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
  combined[recent_skin_cancer == 1, recent_cancer_type := "Melanoma"]
  combined[recent_heme_malign == 1, recent_cancer_type := "Hematologic malignancy"]
  combined[recent_breast_cancer == 1, recent_cancer_type := "Breast cancer"]
  combined[recent_prostate_cancer == 1, recent_cancer_type := "Prostate cancer"]
  combined[recent_lung_cancer == 1, recent_cancer_type := "Lung cancer"]
  combined[is.na(recent_cancer_type) & AnyCancerPhe == 1, recent_cancer_type := "Other cancer"]
  combined[is.na(recent_cancer_type) & AnyCancerPhe == 0, recent_cancer_type := "No cancer"]
  
  # factorize variables and set reference groups -------------
  combined[, recent_cancer_treatment := relevel(factor(recent_cancer_treatment), ref = "No cancer")]
  combined[, recent_cancer_type := relevel(factor(recent_cancer_type), ref = "No cancer")]
  
  # keep only recent variables
  keep_these <- c("id", names(combined)[grepl("recent*", names(combined))])
  
  combined[, ..keep_these]
  
}
