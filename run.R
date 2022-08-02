# top matter ---------
source("libraries.R")
for (f in list.files("src/")) {source(paste0("src/", f))}
source("lists/adjustment_sets.R")

cancer_types <- c("skin_cancer", "heme_malign", "breast_cancer", "prostate_cancer", "lung_cancer", "other_cancer")

# data -----------
whole <- make_main_data()
# whole <- make_main_data(quick_skip = TRUE)
whole <- readRDS("objects/whole_data.rds")
main  <- readRDS("objects/main_data.rds")

# main analyses -----------
any_cancer            <- main_analysis(dataset = "main", exposure_var = "AnyCancerPhe")
any_cancer_time_strat <- main_analysis(dataset = "main", exposure_var = "yscancer_category")

skin_cancer_mods     <- main_analysis(dataset = "main", exposure_var = "skin_cancer_cat")
heme_malign_mods     <- main_analysis(dataset = "main", exposure_var = "heme_malign_cat")
breast_cancer_mods   <- main_analysis(dataset = "main[Sex == 'F']", exposure_var = "breast_cancer_cat")
prostate_cancer_mods <- main_analysis(dataset = "main[Sex == 'M']", exposure_var = "prostate_cancer_cat")
lung_cancer_mods     <- main_analysis(dataset = "main", exposure_var = "lung_cancer_cat")


# cancer_type           <- main_analysis(dataset = "main", exposure_var = cancer_types)
# cancer_type           <- main_analysis(dataset = "main", exposure_var = "cancer_type")
cancer_treatment      <- main_analysis(dataset = "main", exposure_var = "cancer_treatment")
chemo_strat           <- main_analysis(dataset = "main", exposure_var = "chemo_strat_treatment")
no_heme_treatment     <- main_analysis(dataset = "main[heme_malign == 0]", exposure_var = "cancer_treatment")
no_heme_chemo_strat   <- main_analysis(dataset = "main[heme_malign == 0]", exposure_var = "chemo_strat_treatment")

saveRDS(object = any_cancer, file = "objects/any_cancer.rds")
saveRDS(object = any_cancer_time_strat, file = "objects/any_cancer_time_strat.rds")
saveRDS(object = skin_cancer_mods, file = "objects/skin_cancer_mods.rds")
saveRDS(object = heme_malign_mods, file = "objects/heme_malign_mods.rds")
saveRDS(object = breast_cancer_mods, file = "objects/breast_cancer_mods.rds")
saveRDS(object = prostate_cancer_mods, file = "objects/prostate_cancer_mods.rds")
saveRDS(object = lung_cancer_mods, file = "objects/lung_cancer_mods.rds")


# saveRDS(object = cancer_type, file = "objects/cancer_type.rds")
saveRDS(object = cancer_treatment, file = "objects/cancer_treatment.rds")
saveRDS(object = chemo_strat, file = "objects/chemo_stratified.rds")
saveRDS(object = no_heme_treatment, file = "objects/no_heme_treatment.rds")
saveRDS(object = no_heme_chemo_strat, file = "objects/no_heme_chemo_stratified.rds")

results_to_workbook(results = any_cancer)
results_to_workbook(results = any_cancer_time_strat)
results_to_workbook(results = cancer_type)
results_to_workbook(results = cancer_treatment)
results_to_workbook(results = chemo_strat)
results_to_workbook(results = no_heme_treatment)
results_to_workbook(results = no_heme_chemo_strat)

  # interaction analyses -----------
  main_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe", reference_level = "0")
  main_interaction_cancer_reference <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe", reference_level = "1")

  saveRDS(object = main_interaction, file = "objects/main_interaction.rds")  
  saveRDS(object = main_interaction_cancer_reference, file = "objects/main_interaction_cancer_reference.rds")
  
# recent cancer analyses ----------
recent_any_cancer       <- main_analysis(dataset = "main", exposure_var = "recent_AnyCancerPhe")
# recent_cancer_type      <- main_analysis(dataset = "main", exposure_var = paste0("recent_", cancer_types))
# recent_cancer_type      <- main_analysis(dataset = "main", exposure_var = "recent_cancer_type")
recent_cancer_treatment <- main_analysis(dataset = "main", exposure_var = "new_recent_cancer_treatment")

saveRDS(object = recent_any_cancer, file = "objects/recent_any_cancer.rds")
# saveRDS(object = recent_cancer_type, file = "objects/recent_cancer_type.rds")
saveRDS(object = recent_cancer_treatment, file = "objects/recent_cancer_treatment.rds")

results_to_workbook(results = recent_any_cancer)
# results_to_workbook(results = recent_cancer_type)
results_to_workbook(results = recent_cancer_treatment)

  # interaction analyses ----------
  recent_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "yscancer_category", reference_level = "No cancer")
  recent_interaction_cancer_reference <- interaction_analysis(dataset = "main", interaction_var = "yscancer_category", reference_level = "[0,3)")
  
  saveRDS(object = recent_interaction, file = "objects/recent_interaction.rds")  
  saveRDS(object = recent_interaction_cancer_reference, file = "objects/recent_interaction_cancer_reference.rds")

# vaccination analyses ----------
vax_analysis                  <- vaccine_analysis(reference_level = "0")
vax_analysis_cancer_reference <- vaccine_analysis(reference_level = "1")

saveRDS(object = vax_analysis, file = "objects/vax_analysis.rds")
saveRDS(object = vax_analysis_cancer_reference, file = "objects/vax_analysis_cancer_reference.rds")

results_to_workbook(results = vax_analysis)
results_to_workbook(results = vax_analysis_cancer_reference)
  
  
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