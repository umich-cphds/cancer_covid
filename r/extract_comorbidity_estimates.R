## libraries
library(data.table)
library(logistf)
library(ggplot2)
library(purrr)

## no cancer-reference severe covid models with comorbidity score interaction
tmp_sc <- logistf(`Severe COVID` ~ AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + ComorbidityScore:AnyCancerPhe, data = main, control = logistf.control(maxit = 1000))
tmp_hosp <- logistf(Hospitalized ~ AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + ComorbidityScore:AnyCancerPhe, data = main, control = logistf.control(maxit = 1000))
tmp_icu <- logistf(ICU ~ AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + ComorbidityScore:AnyCancerPhe, data = main, control = logistf.control(maxit = 1000))
tmp_death <- logistf(Deceased ~ AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + ComorbidityScore:AnyCancerPhe, data = main, control = logistf.control(maxit = 1000))

## cancer-reference severe covid models with comorbidity score interaction
tmp_sc_cr <- logistf(`Severe COVID` ~ AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + ComorbidityScore:AnyCancerPhe, data = main %>% mutate(AnyCancerPhe = relevel(AnyCancerPhe, ref = "1")), control = logistf.control(maxit = 1000))
tmp_hosp_cr <- logistf(Hospitalized ~ AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + ComorbidityScore:AnyCancerPhe, data = main %>% mutate(AnyCancerPhe = relevel(AnyCancerPhe, ref = "1")), control = logistf.control(maxit = 1000))
tmp_icu_cr <- logistf(ICU ~ AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + ComorbidityScore:AnyCancerPhe, data = main %>% mutate(AnyCancerPhe = relevel(AnyCancerPhe, ref = "1")), control = logistf.control(maxit = 1000))
tmp_death_cr <- logistf(Deceased ~ AnyCancerPhe + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + ComorbidityScore:AnyCancerPhe, data = main %>% mutate(AnyCancerPhe = relevel(AnyCancerPhe, ref = "1")), control = logistf.control(maxit = 1000))

## aggregate results
tmp_res <- list(
  tmp_sc = tmp_sc, tmp_hosp = tmp_hosp, tmp_icu = tmp_icu, tmp_death = tmp_death,
  tmp_sc_cr = tmp_sc_cr, tmp_hosp_cr = tmp_hosp_cr, tmp_icu_cr = tmp_icu_cr, tmp_death_cr = tmp_death_cr)

## format output
tmp_out <- purrr::map2(
  names(tmp_res), c(rep("No cancer", 4), rep("Cancer", 4)),
  ~broomExtra::tidy_parameters(tmp_res[[.x]], ci_method = "wald") %>%
    dplyr::mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) %>%
    dplyr::select(term, estimate, conf_low = conf.low, conf_high = conf.high, p_value = p.value) %>%
    dplyr::mutate(
      OR_print = paste0(
        sprintf("%.2f", round(estimate, 2)),
        " (",
        sprintf("%.2f", round(conf_low, 2)),
        ", ",
        sprintf("%.2f", round(conf_high, 2)), ")")) %>%
    dplyr::relocate(term, estimate, conf_low, conf_high, OR_print, p_value) %>%
  dplyr::mutate(ref = .y)
)

names(tmp_out) <- names(tmp_res)

## save output
saveRDS(tmp_out, "objects/any_cancer_full_results.rds")
