# top matter ---------
source("libraries.R")
purrr::walk(list.files("src/"), ~source(paste0("src/", .x)))
source("lists/adjustment_sets.R")

# cancer_types <- c("skin_cancer", "lymphoid", "myeloid", "kidney_cancer", "bladder_cancer", "colorectal_cancer", "breast_cancer", "prostate_cancer", "lung_cancer", "other_cancer")

cohort_version <- "20220701"

# data -----------
# whole <- make_main_data(save = FALSE)
# saveRDS(whole, paste0("data/whole_data_", cohort_version, ".rds))
# saveRDS(whole[`Test Results` == 1], paste0("data/main_data_", cohort_version, ".rds))
whole <- readRDS(paste0("data/whole_data_", cohort_version, ".rds"))
main  <- readRDS(paste0("data/main_data_", cohort_version, ".rds"))

# main analyses -----------
any_cancer            <- main_analysis(dataset = "main", exposure_var = "AnyCancerPhe")
any_cancer_time_strat <- main_analysis(dataset = "main", exposure_var = "yscancer_category")

skin_cancer_mods       <- main_analysis(dataset = "main", exposure_var = "skin_cancer_cat")
lymphoid_mods          <- main_analysis(dataset = "main", exposure_var = "lymphoid_cat")
myeloid_mods           <- main_analysis(dataset = "main", exposure_var = "myeloid_cat")
bladder_cancer_mods    <- main_analysis(dataset = "main", exposure_var = "bladder_cancer_cat")
kidney_cancer_mods     <- main_analysis(dataset = "main", exposure_var = "kidney_cancer_cat")
colorectal_cancer_mods <- main_analysis(dataset = "main", exposure_var = "colorectal_cancer_cat")
# heme_malign_mods     <- main_analysis(dataset = "main", exposure_var = "heme_malign_cat")
breast_cancer_mods     <- main_analysis(dataset = "main[Sex == 'F']", exposure_var = "breast_cancer_cat")
prostate_cancer_mods   <- main_analysis(dataset = "main[Sex == 'M']", exposure_var = "prostate_cancer_cat")
lung_cancer_mods       <- main_analysis(dataset = "main", exposure_var = "lung_cancer_cat")


# cancer_type           <- main_analysis(dataset = "main", exposure_var = cancer_types)
# cancer_type           <- main_analysis(dataset = "main", exposure_var = "cancer_type")
cancer_treatment      <- main_analysis(dataset = "main", exposure_var = "cancer_treatment")
chemo_strat           <- main_analysis(dataset = "main", exposure_var = "chemo_strat_treatment")
no_heme_treatment     <- main_analysis(dataset = "main[lymphoid == 0 & myeloid == 0]", exposure_var = "cancer_treatment")
no_heme_chemo_strat   <- main_analysis(dataset = "main[lymphoid == 0 & myeloid == 0]", exposure_var = "chemo_strat_treatment")

# save results
results_part_1 <- list(
  any_cancer             = any_cancer,
  any_cancer_time_strat  = any_cancer_time_strat,
  skin_cancer_mods       = skin_cancer_mods,
  lymphoid_mods          = lymphoid_mods,
  myeloid_mods           = myeloid_mods,
  bladder_cancer_mods    = bladder_cancer_mods,
  kidney_cancer_mods     = kidney_cancer_mods,
  colorectal_cancer_mods = colorectal_cancer_mods,
  breast_cancer_mods     = breast_cancer_mods,
  prostate_cancer_mods   = prostate_cancer_mods,
  lung_cancer_mods       = lung_cancer_mods,
  cancer_treatment       = cancer_treatment,
  chemo_strat            = chemo_strat,
  no_heme_treatment      = no_heme_treatment,
  no_heme_chemo_strat    = no_heme_chemo_strat
)

purrr::walk(names(results_part_1),
           ~saveRDS(object = results_part_1[[.x]], file = paste0("objects/", .x, ".rds")))
purrr::walk(names(results_part_1),
            ~fwrite(x = results_part_1[[.x]][["clean"]], file = paste0("objects/", .x, ".csv")))

  # interaction analyses -----------
  main_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe", reference_level = "0")
  main_interaction_cancer_reference <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe", reference_level = "1")

main_int_results <- list(main_interaction = main_interaction, main_interaction_cancer_reference = main_interaction_cancer_reference)
purrr::walk(
    names(main_int_results),
    ~saveRDS(object = main_int_results[[.x]], file = paste0("objects/", .x, ".rds"))
  )
# recent cancer analyses ----------
recent_any_cancer       <- main_analysis(dataset = "main", exposure_var = "recent_AnyCancerPhe")
recent_cancer_treatment <- main_analysis(dataset = "main", exposure_var = "new_recent_cancer_treatment")

recent_res <- list(recent_any_cancer = recent_any_cancer, recent_cancer_treatment = recent_cancer_treatment)
purrr::walk(
  names(recent_res),
  ~saveRDS(object = recent_res[[.x]], file = paste0("objects/", .x, ".rds"))
)
purrr::walk(
  names(recent_res),
  ~fwrite(x = recent_res[[.x]][["clean"]], file = paste0("objects/", .x, ".csv"))
)

  # interaction analyses ----------
  recent_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "yscancer_category", reference_level = "No cancer")
  recent_interaction_cancer_reference <- interaction_analysis(dataset = "main", interaction_var = "yscancer_category", reference_level = "[0,3)")
  
  saveRDS(object = recent_interaction, file = "objects/recent_interaction.rds")  
  saveRDS(object = recent_interaction_cancer_reference, file = "objects/recent_interaction_cancer_reference.rds")

# vaccination analyses ----------
vax_analysis                  <- vaccine_analysis(reference_level = "0")
vax_analysis_cancer_reference <- vaccine_analysis(reference_level = "1")

vax_res <- list(vax_analysis = vax_analysis, vax_analysis_cancer_reference = vax_analysis_cancer_reference)

purrr::walk(names(vax_res), ~saveRDS(object = vax_res[[.x]], file = paste0("objects/", .x, ".rds")))
purrr::walk(names(vax_res), ~fwrite(x = vax_res[[.x]][["clean"]], file = paste0("objects/", .x, ".csv")))
  
vax_ev_sc <- logistf(`Severe COVID` ~ vax_status + factor(AnyCancerPhe) + vax_ever:factor(AnyCancerPhe) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main, control = logistf.control(maxit = 1000))


# vax: ever/never
main[, vax_ever := fifelse(vax_status %in% c("Boosted", "Fully vaccinated (no booster)", "Partially vaccinated"), 1, 0)]

vax_ev_sc <- logistf(`Severe COVID` ~ vax_ever + factor(AnyCancerPhe) + vax_ever:factor(AnyCancerPhe) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main, control = logistf.control(maxit = 1000))
vax_ev_hosp <- logistf(Hospitalized ~ vax_ever + factor(AnyCancerPhe) + vax_ever:factor(AnyCancerPhe) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main, control = logistf.control(maxit = 1000))
vax_ev_icu <- logistf(ICU ~ vax_ever + factor(AnyCancerPhe) + vax_ever:factor(AnyCancerPhe) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main, control = logistf.control(maxit = 1000))
vax_ev_death <- logistf(Deceased ~ vax_ever + factor(AnyCancerPhe) + vax_ever:factor(AnyCancerPhe) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main, control = logistf.control(maxit = 1000))

vax_ev_sc_cr <- logistf(`Severe COVID` ~ vax_ever + AnyCancerPhe + vax_ever:AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main %>% mutate(AnyCancerPhe = relevel(AnyCancerPhe, ref = "1")), control = logistf.control(maxit = 1000))
vax_ev_hosp_cr <- logistf(Hospitalized ~ vax_ever + AnyCancerPhe + vax_ever:AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main %>% mutate(AnyCancerPhe = relevel(AnyCancerPhe, ref = "1")), control = logistf.control(maxit = 1000))
vax_ev_icu_cr <- logistf(ICU ~ vax_ever + AnyCancerPhe + vax_ever:AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main %>% mutate(AnyCancerPhe = relevel(AnyCancerPhe, ref = "1")), control = logistf.control(maxit = 1000))
vax_ev_death_cr <- logistf(Deceased ~ vax_ever + AnyCancerPhe + vax_ever:AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main %>% mutate(AnyCancerPhe = relevel(AnyCancerPhe, ref = "1")), control = logistf.control(maxit = 1000))

vax_ev <- list(
  vax_ev_sc    = vax_ev_sc,
  vax_ev_hosp  = vax_ev_hosp,
  vax_ev_icu   = vax_ev_icu,
  vax_ev_death = vax_ev_death,
  vax_ev_sc_cr    = vax_ev_sc_cr,
  vax_ev_hosp_cr  = vax_ev_hosp_cr,
  vax_ev_icu_cr   = vax_ev_icu_cr,
  vax_ev_death_cr = vax_ev_death_cr
)

tidy_vax_ev <- purrr::map(names(vax_ev), ~tidy_model_output(vax_ev[[.x]]))
names(tidy_vax_ev) <- names(vax_ev)

purrr::walk(names(tidy_vax_ev), ~fwrite(x = tidy_vax_ev[[.x]], file = paste0("objects/", .x, ".csv")))

vax_ev_2020 <- logistf(`Severe COVID` ~ vax_ever + factor(AnyCancerPhe) + vax_ever:factor(AnyCancerPhe) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + i2020 + vax_ever:i2020, data = main %>% mutate(i2020 = relevel(i2020, ref = "2020")), control = logistf.control(maxit = 1000))
vax_ev_2021 <- logistf(`Severe COVID` ~ vax_ever + factor(AnyCancerPhe) + vax_ever:factor(AnyCancerPhe) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + i2020 + vax_ever:i2020, data = main %>% mutate(i2020 = relevel(i2020, ref = "2021")), control = logistf.control(maxit = 1000))
vax_ev_2022 <- logistf(`Severe COVID` ~ vax_ever + factor(AnyCancerPhe) + vax_ever:factor(AnyCancerPhe) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + i2020 + vax_ever:i2020, data = main %>% mutate(i2020 = relevel(i2020, ref = "2022")), control = logistf.control(maxit = 1000))

vax_year <- list(
  vax_ev_2020 = vax_ev_2020,
  vax_ev_2021 = vax_ev_2021,
  vax_ev_2022 = vax_ev_2022
)

tidy_vax_year <- purrr::map(names(vax_year), ~tidy_model_output(vax_year[[.x]]))
names(tidy_vax_year) <- names(vax_year)

purrr::walk(names(tidy_vax_year), ~fwrite(x = tidy_vax_year[[.x]], file = paste0("objects/", .x, ".csv")))


# plots and figures -----------
source("lists/n_by_adjustment_set.R")
  
rmarkdown::render("objects/table1.Rmd")
  
make_bar_plot(data_input = main)
make_recent_bar_plot(data_input = main)

  make_forest_plot(
    no_cancer_results = tidy_table(main_interaction, cr = FALSE),
    cancer_results = tidy_table(main_interaction_cancer_reference, cr = TRUE),
    outcome = "severe_covid"
  )
  make_forest_plot(
    no_cancer_results = tidy_table(main_interaction, cr = FALSE),
    cancer_results = tidy_table(main_interaction_cancer_reference, cr = TRUE),
    outcome = "hospitalization"
  )
  make_forest_plot(
    no_cancer_results = tidy_table(main_interaction, cr = FALSE),
    cancer_results = tidy_table(main_interaction_cancer_reference, cr = TRUE),
    outcome = "icu_admission"
  )
  make_forest_plot(
    no_cancer_results = tidy_table(main_interaction, cr = FALSE),
    cancer_results = tidy_table(main_interaction_cancer_reference, cr = TRUE),
    outcome = "deceased"
  )
  
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE, manual_term = "yscancer_category[0,3)"),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE, manual_term = "yscancer_categoryNo cancer"),
    outcome = "severe_covid",
    other = "_recent"
  )
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE, manual_term = "yscancer_category[0,3)"),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE, manual_term = "yscancer_categoryNo cancer"),
    outcome = "hospitalization",
    other = "_recent"
  )
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE, manual_term = "yscancer_category[0,3)"),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE, manual_term = "yscancer_categoryNo cancer"),
    outcome = "icu_admission",
    other = "_recent"
  )
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE, manual_term = "yscancer_category[0,3)"),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE, manual_term = "yscancer_categoryNo cancer"),
    outcome = "deceased",
    other = "_recent"
  )
  
make_cancer_by_vax_plot(outcome = "`Severe COVID`", title = "severe COVID")
make_cancer_by_vax_plot(outcome = "Hospitalized", title = "hospitalization")
make_cancer_by_vax_plot(outcome = "ICU", title = "ICU admission")
make_cancer_by_vax_plot(outcome = "Deceased", title = "mortality")

# supplement ---------

  # get counts by cancer phecodes used to generate AnyCancerPhe
  source("lists/cancer_counts.R")

  # run `r/get_comorbidity_count.R` to generate numbers for Table S3
