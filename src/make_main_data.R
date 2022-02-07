# Loads, manipulates, and prepares clean data file for analysis
make_main_data <- function(force = FALSE, save = TRUE) {
  
  if (force == FALSE) {
    if (file.exists("objects/main_data.rds")) {
      
      skip <- readline(prompt = "`objects/main_data.rds` already exists. Run anyway (y/n)? ")
      
      if (tolower(substr(as.character(skip), 1, 1)) == "y") {
        stop()
      }
      
    }
  }
  
  # load phecode data ----------
  icd_phecode_tested    <- load_icd_phecode_tested_data()
  icd_phecode_unmatched <- load_icd_phecode_unmatched_data()
  
  icd_phecode_tested    <- sex_concordance_check(x = icd_phecode_tested, tested = TRUE)
  icd_phecode_unmatched <- sex_concordance_check(x = icd_phecode_unmatched, tested = FALSE)
  
  icd_phecode <- rbindlist(list(icd_phecode_tested, icd_phecode_unmatched))
  
  # get ids -----------
  codes <- readRDS("~/projects/covid/new/lists/cancer_codes.rds")
  
  
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
  unmatched_cov_data <- load_unmatched_covariate_data(
    unmatched_phenome_ids = icd_phecode_unmatched[, id],
    cancer_untested_ids   = cancer[tested == 0, id]
  )
  
  # remove unmatched in tested ----------
  unmatched_to_exclude <- unmatched_cov_data[, id][unmatched_cov_data[, id] %in% tested_cov_data[, id]]
  
  unmatched_cov_data <- unmatched_cov_data[id %notin% unmatched_to_exclude]
  
  # cancer treatment data -----------
  cli::cli_alert_info("loading cancer treatment data")
  
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
    tested_cov_data[, .(id, dsb_c19_test_or_dx = dsb_earliest_test_dx_date)],
    unmatched_cov_data[, .(id, dsb_c19_test_or_dx = DaysSinceBirth_EARLIEST_TEST_OR_DX)]))[!is.na(dsb_c19_test_or_dx)]
  
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
                "NDI",
                "PersonsPerSquareMile", "RespiratoryDiseases",
                "CirculatoryDiseases", "AnyCancer", "Type2Diabetes",
                "KidneyDiseases", "LiverDiseases",
                "AutoimmuneDiseases")
  factor_vars <- c("Drinker", "AnyCancerPhe", "AnyCancer",
                   "RespiratoryDiseases", "CirculatoryDiseases",
                   "Type2Diabetes", "KidneyDiseases",
                   "LiverDiseases", "AutoimmuneDiseases")
  
  combined <- rbindlist(list(
    tested_cov_data[, Outcome := OutcomeMax][],
    unmatched_cov_data
  ), fill = TRUE)[, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars][]
  
  combined <- combined[, ComborbidityScore.old := as.numeric(ComorbidityScore)][
    , ComorbidityScore := RespiratoryDiseases + CirculatoryDiseases + Type2Diabetes + KidneyDiseases + LiverDiseases + AutoimmuneDiseases
  ][
    , c("Hospitalized", "ICU", "Deceased", "DaysSinceBirth_EARLIEST_TEST_OR_DX") := lapply(.SD, as.numeric), .SDcols = c("Hospitalized", "ICU", "Deceased", "DaysSinceBirth_EARLIEST_TEST_OR_DX")][
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
        PersonsPerSquareMile.old = PersonsPerSquareMile,
        NDI.old = NDI,
        Age.old = Age
      )
    ][
      , `:=` (
        PersonsPerSquareMile = log(PersonsPerSquareMile + 1),
        NDI = NDI/sd(NDI, na.rm = TRUE),
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
  
  combined <- get_pre_positive_vax_status(x = combined)[in_phenome == 1]
  
  if (save == TRUE) {
    cli::cli_alert_info("saving main_data object")
    saveRDS(object = combined, file = "objects/main_data.rds")
  }
  
  return(combined)
  
}
