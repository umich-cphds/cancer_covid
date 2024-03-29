# top matter ---------
source("libraries.R")
purrr::walk(list.files("src/"), ~source(paste0("src/", .x)))
source("lists/adjustment_sets.R")

cohort_version <- "20220801"
dir.create(paste0("objects/", cohort_version))
dir.create(paste0("data/", cohort_version))

# data -----------
whole <- make_main_data(save = FALSE, chrt = "20220801")
saveRDS(whole, paste0("data/",cohort_version, "/whole_data_", cohort_version, ".rds"))
saveRDS(whole[`Test Results` == 1], paste0("data/", cohort_version, "/main_data_", cohort_version, ".rds"))
whole <- readRDS(paste0("data/", cohort_version,"/whole_data_", cohort_version, ".rds"))
main  <- readRDS(paste0("data/", cohort_version,"/main_data_", cohort_version, ".rds"))

## main analyses -----------
# any cancer
any_cancer            <- main_analysis_2.0(exposure = "AnyCancerPhe", dat = main)
any_cancer_time_strat <- main_analysis_2.0(exposure = "yscancer_category", dat = main)

# by cancer site
skin_cancer_mods       <- main_analysis_2.0(exposure = "skin_cancer_cat", dat = main)
heme_malign_mods       <- main_analysis_2.0(exposure = "heme_malign_cat", dat = main)
lymphoid_mods          <- main_analysis_2.0(exposure = "lymphoid_cat", dat = main)
myeloid_mods           <- main_analysis_2.0(exposure = "myeloid_cat", dat = main)
bladder_cancer_mods    <- main_analysis_2.0(exposure = "bladder_cancer_cat", dat = main)
kidney_cancer_mods     <- main_analysis_2.0(exposure = "kidney_cancer_cat", dat = main)
colorectal_cancer_mods <- main_analysis_2.0(exposure = "colorectal_cancer_cat", dat = main)
breast_cancer_mods     <- main_analysis_2.0(exposure = "breast_cancer_cat", dat = main[Sex == "F"])
prostate_cancer_mods   <- main_analysis_2.0(exposure = "prostate_cancer_cat", dat = main[Sex == "M"])
lung_cancer_mods       <- main_analysis_2.0(exposure = "lung_cancer_cat", dat = main)

# by cancer treatment
cancer_treatment    <- main_analysis_2.0(exposure = "cancer_treatment", dat = main)
chemo_strat         <- main_analysis_2.0(exposure = "chemo_strat_treatment", dat = main)
no_heme_treatment   <- main_analysis_2.0(exposure = "cancer_treatment", dat = main[heme_malign == 0])
no_heme_chemo_strat <- main_analysis_2.0(exposure = "chemo_strat_treatment", dat = main[heme_malign == 0])

# save results
results_part_1 <- list(
  any_cancer             = any_cancer,
  any_cancer_time_strat  = any_cancer_time_strat,
  skin_cancer_mods       = skin_cancer_mods,
  heme_malign_mods       = heme_malign_mods,
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
           ~saveRDS(object = results_part_1[[.x]], file = paste0("objects/", cohort_version, "/", .x, ".rds")))
purrr::walk(names(results_part_1),
            ~fwrite(x = results_part_1[[.x]][["clean"]], file = paste0("objects/", cohort_version, "/", .x, ".csv")))

  # interaction analyses -----------
  main_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe", reference_level = "0")
  main_interaction_cancer_reference <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe", reference_level = "1")

main_int_results <- list(main_interaction = main_interaction, main_interaction_cancer_reference = main_interaction_cancer_reference)
purrr::walk(
    names(main_int_results),
    ~saveRDS(object = main_int_results[[.x]], file = paste0("objects/", cohort_version, "/", .x, ".rds"))
  )
## recent cancer analyses ----------
# any cancer
recent_any_cancer       <- main_analysis_2.0(exposure = "recent_AnyCancerPhe", dat = main)
recent_cancer_treatment <- main_analysis_2.0(exposure = "new_recent_cancer_treatment", dat = main)

recent_res <- list(recent_any_cancer = recent_any_cancer, recent_cancer_treatment = recent_cancer_treatment)
purrr::walk(
  names(recent_res),
  ~saveRDS(object = recent_res[[.x]], file = paste0("objects/", cohort_version, "/", .x, ".rds"))
)
purrr::walk(
  names(recent_res),
  ~fwrite(x = recent_res[[.x]][["clean"]], file = paste0("objects/", cohort_version, "/", .x, ".csv"))
)

  # interaction analyses ----------
  recent_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "yscancer_category", reference_level = "No cancer")
  recent_interaction_cancer_reference <- interaction_analysis(dataset = "main", interaction_var = "yscancer_category", reference_level = "[0,3)")
  
  saveRDS(object = recent_interaction, file = paste0("objects/", cohort_version, "/recent_interaction.rds"))  
  saveRDS(object = recent_interaction_cancer_reference, file = paste0("objects/", cohort_version, "/", "recent_interaction_cancer_reference.rds"))

# vaccination analyses ----------
vax_analysis                  <- vaccine_analysis(reference_level = "0")
vax_analysis_cancer_reference <- vaccine_analysis(reference_level = "1")

vax_res <- list(vax_analysis = vax_analysis, vax_analysis_cancer_reference = vax_analysis_cancer_reference)

purrr::walk(names(vax_res), ~saveRDS(object = vax_res[[.x]], file = paste0("objects/", cohort_version, "/", .x, ".rds")))
purrr::walk(names(vax_res), ~fwrite(x = vax_res[[.x]][["clean"]], file = paste0("objects/", cohort_version, "/", .x, ".csv")))


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

purrr::walk(names(tidy_vax_ev), ~fwrite(x = tidy_vax_ev[[.x]], file = paste0("objects/", cohort_version, "/", .x, ".csv")))

main[, vax_full_plus := fifelse(vax_status %in% c("Boosted", "Fully vaccinated (no booster)"), 1, 0)][is.na(vax_status), vax_full_plus := NA]

heme_vax_sc <- logistf(`Severe COVID` ~ vax_ever + heme_malign_cat + vax_ever:heme_malign_cat + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + i2020 + vax_ever:i2020, data = main %>% mutate(heme_malign_cat = relevel(heme_malign_cat, ref = "Hematologic malignancies")), control = logistf.control(maxit = 1000))
no_heme_vax_sc <- logistf(`Severe COVID` ~ vax_ever + heme_malign_cat + vax_ever:heme_malign_cat + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + i2020 + vax_ever:i2020, data = main %>% mutate(heme_malign_cat = relevel(heme_malign_cat, ref = "Other cancer")), control = logistf.control(maxit = 1000))
no_cancer_vax_sc <- logistf(`Severe COVID` ~ vax_ever + heme_malign_cat + vax_ever:heme_malign_cat + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + i2020 + vax_ever:i2020, data = main %>% mutate(heme_malign_cat = relevel(heme_malign_cat, ref = "No cancer")), control = logistf.control(maxit = 1000))

heme_vax_mods <- list(
  heme_vax_sc = heme_vax_sc,
  no_heme_vax_sc = no_heme_vax_sc,
  no_cancer_vax_sc = no_cancer_vax_sc
)

tidy_heme_vax_mods <- purrr::map(names(heme_vax_mods), ~tidy_model_output(heme_vax_mods[[.x]]))
names(tidy_heme_vax_mods) <- names(heme_vax_mods)

purrr::walk(names(tidy_heme_vax_mods), ~fwrite(x = tidy_heme_vax_mods[[.x]], file = paste0("objects/", cohort_version, "/", .x, ".csv")))


# plots and figures -----------
source("lists/n_by_adjustment_set.R")
  
rmarkdown::render("objects/table1.Rmd", output_file = paste0("/net/wonderland/home/mmsalva/projects/covid/new_cancer/objects/", cohort_version, "/table1.html"))
  
make_bar_plot(data_input = main, chrt_vsn = cohort_version)
make_recent_bar_plot(data_input = main, chrt_vsn = cohort_version)

  make_forest_plot(
    no_cancer_results = tidy_table(main_interaction, cr = FALSE),
    cancer_results = tidy_table(main_interaction_cancer_reference, cr = TRUE),
    outcome = "severe_covid",
    chrt_vsn = cohort_version
  )
  make_forest_plot(
    no_cancer_results = tidy_table(main_interaction, cr = FALSE),
    cancer_results = tidy_table(main_interaction_cancer_reference, cr = TRUE),
    outcome = "hospitalization",
    chrt_vsn = cohort_version
  )
  make_forest_plot(
    no_cancer_results = tidy_table(main_interaction, cr = FALSE),
    cancer_results = tidy_table(main_interaction_cancer_reference, cr = TRUE),
    outcome = "icu_admission",
    chrt_vsn = cohort_version
  )
  make_forest_plot(
    no_cancer_results = tidy_table(main_interaction, cr = FALSE),
    cancer_results = tidy_table(main_interaction_cancer_reference, cr = TRUE),
    outcome = "deceased",
    chrt_vsn = cohort_version
  )
  
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE, manual_term = "yscancer_category[0,3)"),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE, manual_term = "yscancer_categoryNo cancer"),
    outcome = "severe_covid",
    other = "_recent",
    chrt_vsn = cohort_version
  )
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE, manual_term = "yscancer_category[0,3)"),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE, manual_term = "yscancer_categoryNo cancer"),
    outcome = "hospitalization",
    other = "_recent",
    chrt_vsn = cohort_version
  )
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE, manual_term = "yscancer_category[0,3)"),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE, manual_term = "yscancer_categoryNo cancer"),
    outcome = "icu_admission",
    other = "_recent",
    chrt_vsn = cohort_version
  )
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE, manual_term = "yscancer_category[0,3)"),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE, manual_term = "yscancer_categoryNo cancer"),
    outcome = "deceased",
    other = "_recent",
    chrt_vsn = cohort_version
  )
  
  
  cancer_by_vax_sc   <- cancer_by_vax_2.0(outcome = "`Severe COVID`", dat = "main")
  cancer_by_vax_hosp <- cancer_by_vax_2.0(outcome = "Hospitalized", dat = "main")
  cancer_by_vax_icu  <- cancer_by_vax_2.0(outcome = "ICU", dat = "main")
  cancer_by_vax_dea  <- cancer_by_vax_2.0(outcome = "Deceased", dat = "main")
  
  clean_cancer_by_vax <- purrr::map_dfr(list(cancer_by_vax_sc, cancer_by_vax_hosp, cancer_by_vax_icu, cancer_by_vax_dea),
             ~.x[["clean"]])
  fwrite(clean_cancer_by_vax,
         paste0("objects/", cohort_version, "/cancer_by_vax_clean.csv"))
  
  vbc2_plot <- make_vax_by_cancer_plot(d = vax_analysis, dc = vax_analysis_cancer_reference)
  cbv2_plot <- make_cancer_by_vax_plot_2.0(clean_res = clean_cancer_by_vax)
  
  patched <- cbv2_plot/ vbc2_plot
  
  cairo_pdf(filename = paste0("objects/", cohort_version, "/stacked_cancer_vax_plots.pdf"), width = 10, height = 11)
  patched +
    plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(0, .985), plot.tag = element_text(face = "bold", size = 18))
  dev.off()
  
  
  cbv_sc_2020   <- cancer_by_vax_2.0(outcome = "`Severe COVID`", dat = "main[i2020 == '2020']")
  cbv_sc_2021   <- cancer_by_vax_2.0(outcome = "`Severe COVID`", dat = "main[i2020 == '2021']")
  cbv_sc_2022   <- cancer_by_vax_2.0(outcome = "`Severe COVID`", dat = "main[i2020 == '2022']")
  
  years <- c("2020", "2021", "2022")
  cbv_list <- list(cbv_sc_2020, cbv_sc_2021, cbv_sc_2022)
  clean_cbv <- purrr::imap_dfr(cbv_list,
                                        ~.x[[.y]][["clean"]][, year := years[.y]])
  fwrite(clean_cbv,
         paste0("objects/", cohort_version, "/cbv_sc_year_clean.csv"))
  
# supplement ---------

  # get counts by cancer phecodes used to generate AnyCancerPhe
  source("lists/cancer_counts.R")
  source("lists/treatment_patterns_upset_plot.R")

  # run `r/get_comorbidity_count.R` to generate numbers for Table S3
