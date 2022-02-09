# top matter ---------
source("libraries.R")
for (f in list.files("src/")) {source(paste0("src/", f))}
source("lists/adjustment_sets.R")

# data -----------
main <- make_main_data()

# main analyses -----------
any_cancer       <- main_analysis(dataset = "main", exposure_var = "AnyCancerPhe")
cancer_type      <- main_analysis(dataset = "main", exposure_var = "cancer_type")
cancer_treatment <- main_analysis(dataset = "main", exposure_var = "cancer_treatment")

saveRDS(object = any_cancer, file = "objects/any_cancer.rds")
saveRDS(object = cancer_type, file = "objects/cancer_type.rds")
saveRDS(object = cancer_treatment, file = "objects/cancer_treatment.rds")

results_to_workbook(results = any_cancer)
results_to_workbook(results = cancer_type)
results_to_workbook(results = cancer_treatment)

  # interaction analyses -----------
  main_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe")
  main_interaction_cancer_reference <- ""

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
  recent_interaction <- interaction_analysis(dataset = "main", interaction_var = "recent_AnyCancerPhe")
  recent_interaction_cancer_reference <- ""

# vaccination analyses ----------