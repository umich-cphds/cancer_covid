tidy_table <- function(resl, rec = FALSE, cr = FALSE, manual_term = NULL) {
  
  table_names <- c("Age", "  [35, 50)","  [50,65)", "  [65+)", "Female sex", "BMI", "  [10, 18.5)", "  [25,30)", "  [30,200)",
                   "Race/Ethnicity", "  African American/Non-Hispanic", "  Other/Unknown", "Alcohol consumption", "Smoking status", "  Current", "  Past", "Population density", "Disadvantage Index (qrtl)", "Affluence Index (qrtl)", "% Hispanic or foreign-born (qrtl)", "% less than high school diploma (qrtl)", "Comorbidity score", "  Respiratory diseases", "  Circulatory diseases", "  Type 2 diabetes", "  Kidney diseases", "  Liver diseases", "  Autoimmune diseases")
  
  # severe covid
  sev_out <- dplyr::bind_rows(
    extract_results(res_list = resl, result = paste0("sev_int_mods"), var = "Age", terms = c("Age"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "AgeCategory", terms = c("AgeCategory[35,50)", "AgeCategory[50,65)", "AgeCategory[65+)"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "Sex", terms = c("SexF"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = paste0("sev_int_mods"), var = "BMI", terms = c("BMI"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "BMIcategory", terms = c("BMIcategory[10,18.5)", "BMIcategory[25,30)", "BMIcategory[30,100)"), recent = rec, cancer_ref = cr, man_int = manual_term),
    tibble::tibble_row(),
    extract_results(res_list = resl, result = "sev_int_mods", var = "RaceEthnicity4", terms = c("RaceEthnicity4African American / Non-Hispanic", "RaceEthnicity4Other/Unknown"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "Drinker", terms = c("DrinkerYes"), recent = rec, cancer_ref = cr, man_int = manual_term),
    tibble::tibble_row(),
    extract_results(res_list = resl, result = "sev_int_mods", var = "SmokingStatus", terms = c("SmokingStatusCurrent", "SmokingStatusPast"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "popden13_17_qrtl", terms = c("popden13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "disadvantage2_13_17_qrtl", terms = c("disadvantage2_13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "affluence13_17_qrtl", terms = c("affluence13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "ethnicimmigrant13_17_qrtl", terms = c("ethnicimmigrant13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "ped1_13_17_qrtl", terms = c("ped1_13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_int_mods", var = "ComorbidityScore", terms = c("ComorbidityScore"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_comorbid_mods", var = "RespiratoryDiseases", terms = c("RespiratoryDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_comorbid_mods", var = "CirculatoryDiseases", terms = c("CirculatoryDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_comorbid_mods", var = "Type2Diabetes", terms = c("Type2Diabetes1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_comorbid_mods", var = "KidneyDiseases", terms = c("KidneyDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_comorbid_mods", var = "LiverDiseases", terms = c("LiverDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "sev_comorbid_mods", var = "AutoimmuneDiseases", terms = c("AutoimmuneDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term)
  )
  
  # hospitalization
  hos_out <- dplyr::bind_rows(
    extract_results(res_list = resl, result = paste0("hos_int_mods"), var = "Age", terms = c("Age"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "AgeCategory", terms = c("AgeCategory[35,50)", "AgeCategory[50,65)", "AgeCategory[65+)"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "Sex", terms = c("SexF"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = paste0("hos_int_mods"), var = "BMI", terms = c("BMI"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "BMIcategory", terms = c("BMIcategory[10,18.5)", "BMIcategory[25,30)", "BMIcategory[30,100)"), recent = rec, cancer_ref = cr, man_int = manual_term),
    tibble::tibble_row(),
    extract_results(res_list = resl, result = "hos_int_mods", var = "RaceEthnicity4", terms = c("RaceEthnicity4African American / Non-Hispanic", "RaceEthnicity4Other/Unknown"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "Drinker", terms = c("DrinkerYes"), recent = rec, cancer_ref = cr, man_int = manual_term),
    tibble::tibble_row(),
    extract_results(res_list = resl, result = "hos_int_mods", var = "SmokingStatus", terms = c("SmokingStatusCurrent", "SmokingStatusPast"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "popden13_17_qrtl", terms = c("popden13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "disadvantage2_13_17_qrtl", terms = c("disadvantage2_13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "affluence13_17_qrtl", terms = c("affluence13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "ethnicimmigrant13_17_qrtl", terms = c("ethnicimmigrant13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "ped1_13_17_qrtl", terms = c("ped1_13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_int_mods", var = "ComorbidityScore", terms = c("ComorbidityScore"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_comorbid_mods", var = "RespiratoryDiseases", terms = c("RespiratoryDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_comorbid_mods", var = "CirculatoryDiseases", terms = c("CirculatoryDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_comorbid_mods", var = "Type2Diabetes", terms = c("Type2Diabetes1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_comorbid_mods", var = "KidneyDiseases", terms = c("KidneyDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_comorbid_mods", var = "LiverDiseases", terms = c("LiverDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "hos_comorbid_mods", var = "AutoimmuneDiseases", terms = c("AutoimmuneDiseases1"), cancer_ref = cr, man_int = manual_term)
  )
  
  # icu admission
  icu_out <- dplyr::bind_rows(
    extract_results(res_list = resl, result = paste0("icu_int_mods"), var = "Age", terms = c("Age"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "AgeCategory", terms = c("AgeCategory[35,50)", "AgeCategory[50,65)", "AgeCategory[65+)"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "Sex", terms = c("SexF"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = paste0("icu_int_mods"), var = "BMI", terms = c("BMI"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "BMIcategory", terms = c("BMIcategory[10,18.5)", "BMIcategory[25,30)", "BMIcategory[30,100)"), recent = rec, cancer_ref = cr, man_int = manual_term),
    tibble::tibble_row(),
    extract_results(res_list = resl, result = "icu_int_mods", var = "RaceEthnicity4", terms = c("RaceEthnicity4African American / Non-Hispanic", "RaceEthnicity4Other/Unknown"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "Drinker", terms = c("DrinkerYes"), recent = rec, cancer_ref = cr, man_int = manual_term),
    tibble::tibble_row(),
    extract_results(res_list = resl, result = "icu_int_mods", var = "SmokingStatus", terms = c("SmokingStatusCurrent", "SmokingStatusPast"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "popden13_17_qrtl", terms = c("popden13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "disadvantage2_13_17_qrtl", terms = c("disadvantage2_13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "affluence13_17_qrtl", terms = c("affluence13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "ethnicimmigrant13_17_qrtl", terms = c("ethnicimmigrant13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "ped1_13_17_qrtl", terms = c("ped1_13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_int_mods", var = "ComorbidityScore", terms = c("ComorbidityScore"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_comorbid_mods", var = "RespiratoryDiseases", terms = c("RespiratoryDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_comorbid_mods", var = "CirculatoryDiseases", terms = c("CirculatoryDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_comorbid_mods", var = "Type2Diabetes", terms = c("Type2Diabetes1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_comorbid_mods", var = "KidneyDiseases", terms = c("KidneyDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_comorbid_mods", var = "LiverDiseases", terms = c("LiverDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "icu_comorbid_mods", var = "AutoimmuneDiseases", terms = c("AutoimmuneDiseases1"), cancer_ref = cr, man_int = manual_term)
  )
  
  # deceased
  # icu admission
  dea_out <- dplyr::bind_rows(
    extract_results(res_list = resl, result = paste0("dea_int_mods"), var = "Age", terms = c("Age"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "AgeCategory", terms = c("AgeCategory[35,50)", "AgeCategory[50,65)", "AgeCategory[65+)"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "Sex", terms = c("SexF"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = paste0("dea_int_mods"), var = "BMI", terms = c("BMI"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "BMIcategory", terms = c("BMIcategory[10,18.5)", "BMIcategory[25,30)", "BMIcategory[30,100)"), recent = rec, cancer_ref = cr, man_int = manual_term),
    tibble::tibble_row(),
    extract_results(res_list = resl, result = "dea_int_mods", var = "RaceEthnicity4", terms = c("RaceEthnicity4African American / Non-Hispanic", "RaceEthnicity4Other/Unknown"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "Drinker", terms = c("DrinkerYes"), recent = rec, cancer_ref = cr, man_int = manual_term),
    tibble::tibble_row(),
    extract_results(res_list = resl, result = "dea_int_mods", var = "SmokingStatus", terms = c("SmokingStatusCurrent", "SmokingStatusPast"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "popden13_17_qrtl", terms = c("popden13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "disadvantage2_13_17_qrtl", terms = c("disadvantage2_13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "affluence13_17_qrtl", terms = c("affluence13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "ethnicimmigrant13_17_qrtl", terms = c("ethnicimmigrant13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "ped1_13_17_qrtl", terms = c("ped1_13_17_qrtl"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_int_mods", var = "ComorbidityScore", terms = c("ComorbidityScore"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_comorbid_mods", var = "RespiratoryDiseases", terms = c("RespiratoryDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_comorbid_mods", var = "CirculatoryDiseases", terms = c("CirculatoryDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_comorbid_mods", var = "Type2Diabetes", terms = c("Type2Diabetes1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_comorbid_mods", var = "KidneyDiseases", terms = c("KidneyDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_comorbid_mods", var = "LiverDiseases", terms = c("LiverDiseases1"), recent = rec, cancer_ref = cr, man_int = manual_term),
    extract_results(res_list = resl, result = "dea_comorbid_mods", var = "AutoimmuneDiseases", terms = c("AutoimmuneDiseases1"), cancer_ref = cr, man_int = manual_term)
  )

  
  return(list(
    severe_covid = sev_out %>% dplyr::mutate(term = table_names, color = rep(x = c("gray95", "white"), length.out = length(table_names))),
    hospitalization = hos_out %>% dplyr::mutate(term = table_names, color = rep(x = c("gray95", "white"), length.out = length(table_names))),
    icu_admission = icu_out %>% dplyr::mutate(term = table_names, color = rep(x = c("gray95", "white"), length.out = length(table_names))),
    deceased = dea_out %>% dplyr::mutate(term = table_names, color = rep(x = c("gray95", "white"), length.out = length(table_names)))
  ))
  
}