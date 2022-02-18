# top matter ---------
source("libraries.R")
for (f in list.files("src/")) {source(paste0("src/", f))}
source("lists/adjustment_sets.R")

# data -----------
# whole <- make_main_data(quick_skip = TRUE)
whole <- readRDS("objects/whole_data.rds")
main  <- whole[`Test Results` == 1]

# main analyses -----------
any_cancer        <- main_analysis(dataset = "main", exposure_var = "AnyCancerPhe")
cancer_type       <- main_analysis(dataset = "main", exposure_var = "cancer_type")
cancer_treatment  <- main_analysis(dataset = "main", exposure_var = "cancer_treatment")
no_heme_treatment <- main_analysis(dataset = "main[heme_malign == 0]", exposure_var = "cancer_treatment")

saveRDS(object = any_cancer, file = "objects/any_cancer.rds")
saveRDS(object = cancer_type, file = "objects/cancer_type.rds")
saveRDS(object = cancer_treatment, file = "objects/cancer_treatment.rds")
saveRDS(object = no_heme_treatment, file = "objects/no_heme_treatment.rds")

results_to_workbook(results = any_cancer)
results_to_workbook(results = cancer_type)
results_to_workbook(results = cancer_treatment)
results_to_workbook(results = no_heme_treatment)

  # interaction analyses -----------
  main_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe", reference_level = "0")
  main_interaction_cancer_reference <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe", reference_level = "1")

  saveRDS(object = main_interaction, file = "objects/main_interaction.rds")  
  saveRDS(object = main_interaction_cancer_reference, file = "objects/main_interaction_cancer_reference.rds")
  
# recent cancer analyses ----------
recent_any_cancer       <- main_analysis(dataset = "main", exposure_var = "recent_AnyCancerPhe")
recent_cancer_type      <- main_analysis(dataset = "main", exposure_var = "recent_cancer_type")
recent_cancer_treatment <- main_analysis(dataset = "main", exposure_var = "recent_cancer_treatment")

saveRDS(object = recent_any_cancer, file = "objects/recent_any_cancer.rds")
saveRDS(object = recent_cancer_type, file = "objects/recent_cancer_type.rds")
saveRDS(object = recent_cancer_treatment, file = "objects/recent_cancer_treatment.rds")

results_to_workbook(results = recent_any_cancer)
results_to_workbook(results = recent_cancer_type)
results_to_workbook(results = recent_cancer_treatment)

  # interaction analyses ----------
  recent_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "recent_AnyCancerPhe", reference_level = "0")
  recent_interaction_cancer_reference <- interaction_analysis(dataset = "main", interaction_var = "recent_AnyCancerPhe", reference_level = "1")
  
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
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE),
    outcome = "severe_covid",
    other = "_recent"
  )
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE),
    outcome = "hospitalization",
    other = "_recent"
  )
  make_forest_plot(
    no_cancer_results = tidy_table(recent_interaction, rec = TRUE, cr = FALSE),
    cancer_results = tidy_table(recent_interaction_cancer_reference, rec = TRUE, cr = TRUE),
    outcome = "icu_admission",
    other = "_recent"
  )
  
make_cancer_by_vax_plot(outcome = "`Severe COVID`", title = "severe COVID")
make_cancer_by_vax_plot(outcome = "Hospitalized", title = "hospitalization")
make_cancer_by_vax_plot(outcome = "ICU", title = "ICU admission")
make_cancer_by_vax_plot(outcome = "Deceased", title = "mortality")

# supplement ---------

  # get counts by cancer phecodes used to generate AnyCancerPhe
  source("lists/cancer_counts.R")