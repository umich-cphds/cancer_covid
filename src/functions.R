# %notin% ----------
`%notin%` <- Negate(`%in%`)

# sex concordance check ----------
sex_concordance_check <- function(x, cohort = "20220202", tested = TRUE) {

  # phecode data
  pheinfo <- get(load(file = "/net/junglebook/michiganmedicine/larsf/data/phenomes/Phecode_Definitions_1.2_Full.Rsav"))
  sex_filter <- pheinfo[, .(phecode, phecode_sex = sex)]

  # demographic data
  if (tested == TRUE) {

    if (cohort == "20220202") {
      demo_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220202/MichiganMedicine_PCRcohort_plus_VaccinationData_20220202.Rsav"
    }
    
    if (cohort == "20220101") {
      demo_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220101/MichiganMedicine_PCRcohort_plus_VaccinationData_20220101.Rsav"
    }

    if (cohort == "20211001") {
      demo_path <- "/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20211001/MichiganMedicine_PCRcohort_plus_VaccinationData_20211001.Rsav"
    }

    demo_sex <- unique(get(load(demo_path))[
      , .(id = Encrypted_PatientID, demo_sex = Sex)])

  } else {

    demo_sex <- unique(fread("/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_4705_RandomControls/HPI_4705_unmatched_ctrls_Demographics.txt")[
      , .(id = Deid_ID, demo_sex = Sex)])

  }

  x <- merge.data.table(x, demo_sex, by = "id")
  x <- merge.data.table(x, sex_filter, by = "phecode")

  cli::cli_alert_success("merged. filtering...")

  x <- x[!((demo_sex == "M" & phecode_sex == "Female") | (demo_sex == "F" & phecode_sex == "Male"))][, !c("demo_sex", "phecode_sex")][]

  return(x)

}

# get ids ----------
get_ids <- function(x, filter_codes) {

  x[phecode %in% filter_codes, unique(id)]

}

# quick and simple code filter -----------
code_filter <- function(x, codes) {
  x[ProcedureCode %in% codes | grepl(paste0("^", codes, "[^0-9]", sep = "", collapse = "|"), ProcedureCode)][]
}

# get last days since birth ----------
get_last_dsb <- function(cpt_dat, icd_dat, c19_dsb, treatment) {
  
  tmp_cpt_dat <- cpt_dat[, .SD[ProcedureDate_DaysSinceBirth == max(ProcedureDate_DaysSinceBirth, na.rm = TRUE)], by = "id"]
  tmp_icd_dat <- icd_dat[, .SD[DaysSinceBirth == max(DaysSinceBirth, na.rm = TRUE)], by = "id"]
  
  tmp <- merge.data.table(
    tmp_cpt_dat[, .(id, cpt_dsb = ProcedureDate_DaysSinceBirth)],
    tmp_icd_dat[, .(id, icd_dsb = DaysSinceBirth)],
    by = "id"
  )[, dsb_last := pmax(cpt_dsb, icd_dsb, na.rm = TRUE)][]
  
  tmp <- merge.data.table(
    tmp,
    c19_dsb,
    by = "id"
  )[, treatment_2years := fifelse((dsb_last - dsb_c19_test_or_dx) < -730, 1, 0)][
    , treatment_2years := fifelse(is.na(treatment_2years), 0, treatment_2years)
  ]
  
  tmp <- unique(tmp)
  
  setnames(tmp, old = c("dsb_last", "treatment_2years"), new = c(paste0("dsb_last_", treatment), paste0(treatment, "_2years")))
  
  return(tmp)
  
}





# get ids for individuals diagnosed with COVID-19 in 2020 -----------
get_2020indicator_ids <- function(x) {
  
  q1_ids <- x[Outcome2_2020.1 %in% c("Mild", "Severe"), id]
  q2_ids <- x[Outcome2_2020.2 %in% c("Mild", "Severe"), id]
  q3_ids <- x[Outcome2_2020.3 %in% c("Mild", "Severe"), id]
  q4_ids <- x[Outcome2_2020.4 %in% c("Mild", "Severe"), id]
  
  unique(c(q1_ids, q2_ids, q3_ids, q4_ids))
  
}

get_2021indicator_ids <- function(x) {
  
  q1_ids <- x[Outcome2_2021.1 %in% c("Mild", "Severe"), id]
  q2_ids <- x[Outcome2_2021.2 %in% c("Mild", "Severe"), id]
  q3_ids <- x[Outcome2_2021.3 %in% c("Mild", "Severe"), id]
  q4_ids <- x[Outcome2_2021.4 %in% c("Mild", "Severe"), id]
  
  unique(c(q1_ids, q2_ids, q3_ids, q4_ids))
  
}

###########
### OLD ###
###########

# load_unmatched_covariate_data <- function(unmatched_phenome_ids, cancer_untested_ids) {
#   
#   out <- fread("/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/PreviousDatasets/MichiganMedicine_COVID19DATA_SIMPLIFIED_w_quarter_20200908.txt")[`Cohort Type` == "Unmatched Controls", -c(16:49, 52:55, 76, 77)][]
#   
#   setnames(out, c("Deid_ID", "RaceEthnicity", "StillInHospialICU", "NeighborhoodSocioeconomicDisadvantageIndex"), c("id", "RaceEthnicity4", "StillInHospitalICU", "NDI"))
#   
#   out <- out[, `:=` (
#     `Test Results` = 0,
#     BMIcategory = data.table::fcase(
#       BMIcategory == "[0,18.5)", "[10,18.5)",
#       BMIcategory == "[18.5,25)", "[18.5,25)",
#       BMIcategory == "[25,30)", "[25,30)",
#       BMIcategory == "[30,200)", "[30,100)"
#     ),
#     in_phenome = fifelse(id %in% unmatched_phenome_ids, 1, 0)
#   )][, AnyCancerPhe := fcase(
#     in_phenome == 1 & id %in% cancer_untested_ids, 1,
#     in_phenome == 1 & id %notin% cancer_untested_ids, 0)][]
#   
#   return(out)
#   
# }
# 
# # print tidy model output ----------
# tidy_model_output <- function(mod, kable_digits = NULL) {
#   
#   out <- broomExtra::tidy_parameters(mod, df_method = "wald") %>%
#     dplyr::mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) %>%
#     dplyr::select(term, estimate, conf_low = conf.low, conf_high = conf.high, p_value = p.value) %>%
#     dplyr::mutate(
#       OR_print = paste0(
#         sprintf("%.2f", round(estimate, 2)),
#         " (",
#         sprintf("%.2f", round(conf_low, 2)),
#         ", ",
#         sprintf("%.2f", round(conf_high, 2)), ")")) %>%
#     dplyr::relocate(term, estimate, conf_low, conf_high, OR_print, p_value)
#   
#   if (!is.null(kable_digits)) {
#     out <- out %>% kable(digits = kable_digits)
#   }
#   
#   return(out)
#   
# }
# 
# # Firth-bias corrected logistic regression analysis pipeline -----------
# tidy_analysis <- function(data, exposure, outcome, covariates, env_name = env_name, int = NULL, kable_digits = NULL) {
#   
#   tmp_covariates <- covariates[!covariates == exposure]
#   
#   if (exposure %in% c("RespiratoryDiseases", "CirculatoryDiseases", "Type2Diabetes",
#                       "KidneyDiseases", "LiverDiseases", "AutoimmuneDiseases")) {
#     
#     if (is.null(int)) {
#       mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')}, data = {data} %>% mutate(ComorbidityScore = ComorbidityScore - as.numeric(as.character({exposure}))))")
#     } else {
#       mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')} + {int}:{exposure}, data = {data} %>% mutate(ComorbidityScore = ComorbidityScore - as.numeric(as.character({exposure}))))")
#     }
#     
#   } else {
#     
#     if (is.null(int)) {
#       mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')}, data = {data})")
#     } else {
#       mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')} + {int}:{exposure}, data = {data})")
#     }
#     
#   }
#   
#   
#   
#   tmp_mod <- eval(parse(text = mod_text), envir = env_name)
#   
#   tidy_out <- tidy_model_output(mod = tmp_mod, kable_digits = kable_digits)
#   
#   list(
#     model       = mod_text,
#     tidy_output = tidy_out,
#     nobs        = nobs(tmp_mod)
#   )
#   
# }
# 
# # multiple models ---------
# tidy_cascade_analysis <- function(
#   data, exposure, outcome, env_name = env_name, sex = "both", kable_digits = NULL
# ) {
#   
#   # adjustment sets -----------
#   if (tolower(sex) %in% c("female", "male", "f", "m")) {
#     
#     adj1 <- c("Age", "factor(RaceEthnicity4)")
#     if (grepl("test", outcome, ignore.case = TRUE) == TRUE) {
#       adj1 <- c(adj1, "popden13_17_qrtl")
#     }
#     
#     adj2 <- c(adj1, "disadvantage2_13_17_qrtl")
#     adj3 <- c(adj2, "ComorbidityScore")
#     
#   } else {
#     
#     adj1 <- c("Age", "factor(Sex)", "factor(RaceEthnicity4)")
#     if (grepl("test", outcome, ignore.case = TRUE) == TRUE) {
#       adj1 <- c(adj1, "popden13_17_qrtl")
#     }
#     
#     adj2 <- c(adj1, "disadvantage2_13_17_qrtl")
#     adj3 <- c(adj2, "ComorbidityScore")
#     
#   }
#   
#   # model text ------------
#   if (tolower(sex) %in% c("male", "m")) {
#     
#     glm_mod_text     <- glue::glue("glm({outcome} ~ factor({exposure}), data = {data}[Sex == 'M'], family = 'binomial')")
#     logistf_mod_text <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}), data = {data}[Sex == 'M'])")
#     adj1_mod_text    <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}) + {paste(adj1, collapse = ' + ')}, data = {data}[Sex == 'M'], control = logistf.control(maxit = 1000))")
#     adj2_mod_text    <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}) + {paste(adj2, collapse = ' + ')}, data = {data}[Sex == 'M'], control = logistf.control(maxit = 1000))")
#     adj3_mod_text    <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}) + {paste(adj3, collapse = ' + ')}, data = {data}[Sex == 'M'], control = logistf.control(maxit = 1000))")
#     
#   } else if (tolower(sex) %in% c("female", "f")) {
#     
#     glm_mod_text     <- glue::glue("glm({outcome} ~ factor({exposure}), data = {data}[Sex == 'F'], family = 'binomial')")
#     logistf_mod_text <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}), data = {data}[Sex == 'F'])")
#     adj1_mod_text    <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}) + {paste(adj1, collapse = ' + ')}, data = {data}[Sex == 'F'], control = logistf.control(maxit = 1000))")
#     adj2_mod_text    <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}) + {paste(adj2, collapse = ' + ')}, data = {data}[Sex == 'F'], control = logistf.control(maxit = 1000))")
#     adj3_mod_text    <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}) + {paste(adj3, collapse = ' + ')}, data = {data}[Sex == 'F'], control = logistf.control(maxit = 1000))")
#     
#   } else {
#     
#     glm_mod_text     <- glue::glue("glm({outcome} ~ factor({exposure}), data = {data}, family = 'binomial')")
#     logistf_mod_text <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}), data = {data})")
#     adj1_mod_text    <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}) + {paste(adj1, collapse = ' + ')}, data = {data}, control = logistf.control(maxit = 1000))")
#     adj2_mod_text    <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}) + {paste(adj2, collapse = ' + ')}, data = {data}, control = logistf.control(maxit = 1000))")
#     adj3_mod_text    <- glue::glue("logistf::logistf({outcome} ~ factor({exposure}) + {paste(adj3, collapse = ' + ')}, data = {data}, control = logistf.control(maxit = 1000))")
#     
#   }
#   
#   # models -----------
#   glm_mod     <- eval(parse(text = glm_mod_text), envir = env_name)
#   logistf_mod <- eval(parse(text = logistf_mod_text), envir = env_name)
#   adj1_mod    <- eval(parse(text = adj1_mod_text), envir = env_name)
#   adj2_mod    <- eval(parse(text = adj2_mod_text), envir = env_name)
#   adj3_mod    <- eval(parse(text = adj3_mod_text), envir = env_name)
#   
#   # format output -----------
#   unadjusted       <- tidy_model_output(mod = glm_mod, kable_digits = kable_digits)
#   unadjusted_firth <- tidy_model_output(mod = logistf_mod, kable_digits = kable_digits)
#   adjustment1      <- tidy_model_output(mod = adj1_mod, kable_digits = kable_digits)
#   adjustment2      <- tidy_model_output(mod = adj2_mod, kable_digits = kable_digits)
#   adjustment3      <- tidy_model_output(mod = adj3_mod, kable_digits = kable_digits)
#   
#   list(
#     unadjusted       = unadjusted |> mutate(nobs = nobs(glm_mod)),
#     unadjusted_firth = unadjusted_firth |> mutate(nobs = nobs(logistf_mod)),
#     adjustment1      = adjustment1 |> mutate(nobs = nobs(adj1_mod)),
#     adjustment2      = adjustment2 |> mutate(nobs = nobs(adj2_mod)),
#     adjustment3      = adjustment3 |> mutate(nobs = nobs(adj3_mod))
#   )
#   
# }
# 
# # summary output -----------
# cli_if_else <- function(condition, if_true, if_false) {
#   
#   if (condition == TRUE) {
#     cli::cli_alert_success(if_true)
#   } else {
#     cli::cli_alert_danger(if_false)
#   }
#   
#   
# }
# 
# # make analytic datasets ----------
# make_positive_unmatched <- function(x, cancer = FALSE, cancer_ref = FALSE, no_heme = FALSE) {
#   
#   tmp <- x %>%
#     dplyr::filter(`Test Results` == 1 | `Cohort Type` == "Unmatched Controls")
#   
#   if (cancer == TRUE) {
#     tmp <- tmp %>% dplyr::filter(AnyCancerPhe == 1)
#   }
#   
#   if (no_heme == TRUE) {
#     tmp <- tmp %>% dplyr::filter(heme_malign != 1)
#   }
#   
#   if (cancer_ref == TRUE) {
#     tmp <- tmp %>% dplyr::mutate(AnyCancerPhe = as.factor(AnyCancerPhe) %>% relevel(ref = "1"))
#   }
#   
#   return(tmp)
#   
# }
# 
# make_negative_unmatched <- function(x, cancer = FALSE, no_heme = FALSE) {
#   
#   tmp <- x %>%
#     dplyr::filter(`Test Results` == 0 | `Cohort Type` == "Unmatched Controls") %>%
#     dplyr::mutate(tested_indicator = case_when(
#       `Cohort Type` == "Unmatched Controls" ~ 0,
#       T ~ 1
#     ))
#   
#   if (cancer == TRUE) {
#     tmp <- tmp %>% dplyr::filter(AnyCancerPhe == 1)
#   }
#   
#   if (no_heme == TRUE) {
#     tmp <- tmp %>% dplyr::filter(heme_malign != 1)
#   }
#   
#   return(tmp)
# }
# 
# make_tested <- function(x, cancer = FALSE, no_heme = FALSE) {
#   
#   tmp <- x %>%
#     dplyr::filter(`Cohort Type` == "Tested")
#   
#   if (cancer == TRUE) {
#     tmp <- tmp %>% dplyr::filter(AnyCancerPhe == 1)
#   }
#   
#   if (no_heme == TRUE) {
#     tmp <- tmp %>% dplyr::filter(heme_malign != 1)
#   }
#   
#   return(tmp)
#   
# }
# 
# make_tested_positive <- function(x, cancer = FALSE, cancer_ref = FALSE, no_heme = FALSE) {
#   
#   tmp <- x %>% dplyr::filter(`Test Results` == 1)
#   
#   if (cancer == TRUE) {
#     tmp <- tmp %>% dplyr::filter(AnyCancerPhe == 1)
#   }
#   
#   if (no_heme == TRUE) {
#     tmp <- tmp %>% dplyr::filter(heme_malign != 1)
#   }
#   
#   if (cancer_ref == TRUE) {
#     tmp <- tmp %>% dplyr::mutate(AnyCancerPhe = as.factor(AnyCancerPhe) %>% relevel(ref = "1"))
#   }
#   
#   return(tmp)
#   
# }
# 
# make_positive_cancer <- function(x, no_heme = FALSE) {
#   
#   if (no_heme == FALSE) {
#     x %>% dplyr::filter(`Test Results` == 1 & AnyCancerPhe == 1)
#   } else {
#     x %>% dplyr::filter(`Test Results` == 1 & AnyCancerPhe == 1 & heme_malign != 1)
#   }
#   
# }
# 
# # extract estimates from tidy_cascade_analysis ------------
# extract_tidy_cascade_estimates <- function(tca_res, outcome, mod_term = "factor(AnyCancerPhe)1") {
#   
#   f <- names(tca_res)
#   
#   for (i in seq_along(f)) {
#     
#     if (i == 1) {
#       tmp_out <- tca_res[[f[i]]] %>%
#         dplyr::filter(term == mod_term) %>%
#         mutate(model = f[i]) %>%
#         dplyr::select(model, term, estimate, conf_low, conf_high, OR_print, p_value, nobs)
#     } else {
#       tmp_out <- bind_rows(tmp_out,
#                            tca_res[[f[i]]] %>%
#                              dplyr::filter(term == mod_term) %>%
#                              mutate(model = f[i]) %>%
#                              dplyr::select(model, term, estimate, conf_low, conf_high, OR_print, p_value, nobs))
#     }
#     
#   }
#   
#   tidy_out <- tmp_out %>% select(model, OR_print)
#   names(tidy_out)[names(tidy_out) == "OR_print"] <- outcome
#   
#   list(
#     full = tmp_out,
#     tidy = tidy_out
#   )
#   
# }
# 