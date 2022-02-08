# top matter ---------
source("libraries.R")
for (f in list.files("src/")) {source(paste0("src/", f))}
source("lists/adjustment_sets.R")

# data -----------
main <- make_main_data()

# main analyses -----------
any_cancer       <- main_analysis(dataset = main, exposure_var = "AnyCancerPhe")
cancer_type      <- main_analysis(dataset = main, exposure_var = "cancer_type")
cancer_treatment <- main_analysis(dataset = main, exposure_var = "cancer_treatment")

  # interaction analyses -----------
  main_interaction                  <- interaction_analysis(dataset = "main", interaction_var = "AnyCancerPhe")
  main_interaction_cancer_reference <- ""

# recent cancer analyses ----------
recent_any_cancer       <- main_analysis(dataset = main, exposure_var = "recent_AnyCancerPhe")
recent_cancer_type      <- main_analysis(dataset = main, exposure_var = "recent_cancer_type")
recent_cancer_treatment <- main_analysis(dataset = main, exposure_var = "recent_cancer_treatment")

  # interaction analyses ----------
  recent_interaction <- interaction_analysis(dataset = "main", interaction_var = "recent_AnyCancerPhe")
  recent_interaction_cancer_reference <- ""

# vaccination analyses ----------