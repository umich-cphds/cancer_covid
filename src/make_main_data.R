# Loads, manipulates, and prepares clean data file for analysis
make_main_data <- function(force = FALSE, save = TRUE, quick_skip = FALSE) {
  
  if (force == FALSE) {
    if (file.exists("objects/whole_data.rds") & quick_skip == FALSE) {
      
      skip <- readline(prompt = "`objects/whole_data.rds` already exists. Run anyway (y/n)? ")
      
      if (tolower(substr(as.character(skip), 1, 1)) == "n") {
        stop()
      }
      
      cli::cli_alert_info("running `make_main_data`...")
      
    }
    
    if (file.exists("objects/whole_data.rds") & quick_skip == TRUE) {
      cli::cli_alert_info("`objects/whole_data.rds` exists. skipping data processing.")
      readRDS("objects/whole_data.rds")
    }
    
  }
  
  # load phecode data ----------
  icd_phecode_tested    <- load_icd_phecode_tested_data()
  icd_phecode           <- sex_concordance_check(x = icd_phecode_tested, tested = TRUE)
  
  # get ids -----------
  codes <- readRDS("~/projects/covid/new_cancer/lists/cancer_codes.rds")
  
  skin_cancer_ids     <- get_ids(x = icd_phecode, filter_codes = codes$skin_cancer_phecodes)
  heme_malign_ids     <- get_ids(x = icd_phecode, filter_codes = codes$heme_malign_phecodes)
  breast_cancer_ids   <- get_ids(x = icd_phecode, filter_codes = codes$breast_cancer_phecodes)
  prostate_cancer_ids <- get_ids(x = icd_phecode, filter_codes = codes$prostate_cancer_phecodes)
  lung_cancer_ids     <- get_ids(x = icd_phecode, filter_codes = codes$lung_cancer_phecodes)
  
  cancer <- icd_phecode[phecode %in% codes$cancer_phecodes]
  
  # load covariate data -----------
  outcomes <- readRDS("~/projects/covid/new_cancer/lists/outcomes.rds")
  
  tested_cov_data    <- load_tested_covariate_data(
    outvar             = outcomes,
    tested_phenome_ids = icd_phecode_tested[, id],
    cancer_tested_ids  = cancer[tested == 1, id]
  )
  
  # cancer treatment data -----------
  cli::cli_alert_info("loading cancer treatment data")
  
  unmatched_to_exclude <- c("")
  
  procedures <- load_procedure_data(exclude_ids = unmatched_to_exclude)
  
  chemo_icd <- icd_phecode[DiagnosisCode %in% codes$chemo$chemo_icd_codes]
  rad_icd   <- icd_phecode[DiagnosisCode %in% codes$radiation$radiation_icd_codes]
  imt_icd   <- icd_phecode[DiagnosisCode %in% codes$immunotherapy$immunotherapy_icd_codes]
  
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
  
  # calculate last dates -----------
  cli::cli_alert_info("calculating last date of cancer treatment")
  
  c19_test_or_dx <- rbindlist(list(
    tested_cov_data[, .(id, dsb_c19_test_or_dx = dsb_earliest_test_dx_date)]))[!is.na(dsb_c19_test_or_dx)]
  
  last_radiation <- get_last_dsb(cpt_dat = rad_cpt, icd_dat = rad_icd, c19_dsb = c19_test_or_dx, treatment = "radiation")
  last_chemo <- get_last_dsb(cpt_dat = chemo_cpt, icd_dat = chemo_icd, c19_dsb = c19_test_or_dx, treatment = "chemo")
  
  last_surgery <- surgery[, .SD[ProcedureDate_DaysSinceBirth == max(ProcedureDate_DaysSinceBirth, na.rm = T)], by = "id"][, .(id, dsb_last_surgery = ProcedureDate_DaysSinceBirth)][]
  last_surgery <- merge.data.table(
    last_surgery,
    c19_test_or_dx,
    by = "id"
  )[, surgery_2years := fifelse((dsb_last_surgery - dsb_c19_test_or_dx) < -730, 1, 0)][
    , surgery_2years := fifelse(is.na(surgery_2years), 0, surgery_2years)
  ][, .(id, dsb_last_surgery, surgery_2years)][]
  
  last_cancer <- cancer[, .SD[DaysSinceBirth == max(DaysSinceBirth, na.rm = T)], by = "id"][, .(id, dsb_last_cancer = DaysSinceBirth)]
  last_cancer <- merge.data.table(
    last_cancer,
    c19_test_or_dx,
    by = "id"
  )[, cancer_2years := fifelse((dsb_last_cancer - dsb_c19_test_or_dx) < -730, 1, 0)][
    , cancer_2years := fifelse(is.na(cancer_2years), 0, cancer_2years)
  ][, .(id, dsb_last_cancer, cancer_2years)][]
  
  last_active_cancer <- cancer[DiagnosisCode %notin% codes$history_of_cancer_icd_codes][, .SD[DaysSinceBirth == max(DaysSinceBirth, na.rm = T)], by = "id"][, .(id, dsb_last_active_cancer = DaysSinceBirth)]
  last_active_cancer <- merge.data.table(
    last_active_cancer,
    c19_test_or_dx,
    by = "id"
  )[, active_cancer_2years := fifelse((dsb_last_active_cancer - dsb_c19_test_or_dx) < -730, 1, 0)][
    , active_cancer_2years := fifelse(is.na(active_cancer_2years), 0, active_cancer_2years)
  ][, .(id, dsb_last_active_cancer, active_cancer_2years)][]
  
  last_dates <- merge.data.table(unique(last_radiation), unique(last_chemo), all = TRUE)
  last_dates <- merge.data.table(unique(last_dates), unique(last_surgery), all = TRUE)
  last_dates <- merge.data.table(unique(last_dates), unique(last_cancer), all = TRUE)
  last_dates <- merge.data.table(unique(last_dates), unique(last_active_cancer), all = TRUE)
  
  # combining data
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
  
  combined <- tested_cov_data[in_phenome == 1][, Outcome := OutcomeMax][, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars]
  
  combined <- combined[, ComborbidityScore.old := as.numeric(ComorbidityScore)][
    , ComorbidityScore := RespiratoryDiseases + CirculatoryDiseases + Type2Diabetes + KidneyDiseases + LiverDiseases + AutoimmuneDiseases
  ][
    , c("Hospitalized", "ICU", "Deceased") := lapply(.SD, as.numeric), .SDcols = c("Hospitalized", "ICU", "Deceased")][
      , `:=` (
        AgeCategory    = relevel(as.factor(AgeCategory), ref = "[18,35)"),
        BMIcategory    = relevel(as.factor(BMIcategory), ref = "[18.5,25)"),
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
        Age = Age/10
      )
    ][, `:=` (
      skin_cancer     = fifelse(id %in% skin_cancer_ids, 1, 0),
      heme_malign     = fifelse(id %in% heme_malign_ids, 1, 0),
      breast_cancer   = fifelse(id %in% breast_cancer_ids, 1, 0),
      prostate_cancer = fifelse(id %in% prostate_cancer_ids, 1, 0),
      lung_cancer     = fifelse(id %in% lung_cancer_ids, 1, 0)
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
  
  combined <- merge.data.table(
    combined,
    last_dates,
    by = "id",
    all = TRUE
  )
  
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
  combined[skin_cancer == 1, cancer_type := "Melanoma"]
  combined[heme_malign == 1, cancer_type := "Hematologic malignancy"]
  combined[breast_cancer == 1, cancer_type := "Breast cancer"]
  combined[prostate_cancer == 1, cancer_type := "Prostate cancer"]
  combined[lung_cancer == 1, cancer_type := "Lung cancer"]
  combined[is.na(cancer_type) & AnyCancerPhe == 1, cancer_type := "Other cancer"]
  combined[is.na(cancer_type) & AnyCancerPhe == 0, cancer_type := "No cancer"]
  
  # factorize variables and set reference groups -------------
  combined[, cancer_treatment := relevel(factor(cancer_treatment), ref = "No cancer")]
  combined[, cancer_type := relevel(factor(cancer_type), ref = "No cancer")]
  combined[, vax_status := relevel(factor(vax_status), ref = "Unvaccinated/Unknown")]
  
  # combine categories and update vax definition
  combined[RaceEthnicity4 %in% c("Other / Non-Hispanic or Hispanic", "Other / Unknown Ethnicity"), RaceEthnicity4 := "Other/Unknown"][, RaceEthnicity4 := relevel(factor(RaceEthnicity4), ref = "Caucasian / Non-Hispanic")]
  combined[vax_status == "Unvaccinated/Unknown", vax_status = "Prior to vaccination"]
  combined[vax_status == "Partially vaccinated", vax_status = "After one dose"]
  combined[vax_status == "Fully vaccinated", vax_status = "After two doses"]
  combined[vax_status == "Boosted", vax_status = "After booster"]
  combined[, vax_status := relevel(factor(vax_status), "Prior to vaccination")]
  
  # add recent cancer variables -----------
  cli::cli_alert_info("constructing recent cancer variables...")
  restricted <- make_restricted_phenome_data(input = combined)
  
  # create indicator variable for COVID-19 cases diagnosed in 2020 ------------
  i2020_ids <- get_2020indicator_ids(x = combined)
  combined[, i2020 := fifelse(id %in% i2020_ids, 1, 0)]
  
  combined <- merge.data.table(
    combined,
    restricted,
    by = "id", all.x = TRUE
  )
  
  if (save == TRUE) {
    cli::cli_alert_info("saving whole_data object")
    saveRDS(object = combined, file = "objects/whole_data.rds")
    cli::cli_alert_info("saving main_data object (tested positive only)")
    saveRDS(object = combined[`Test Results` == 1], file = "objects/main_data.rds")
  }
  
  return(combined)
  
}
