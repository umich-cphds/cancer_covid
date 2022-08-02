vaccine_cascade <- function(outcome, cancer_var = "AnyCancerPhe", env_name = .GlobalEnv, kable_digits = NULL, ref_level = "0") {
  
  glm_mod_text <- glue("glm({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}), data = main %>% mutate({cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), family = 'binomial')")
  logistf_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}), data = main %>% mutate({cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  adj1_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + Age + factor(Sex) + factor(RaceEthnicity4) + factor(i2020), data = main %>% mutate({cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  adj2_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + factor(i2020), data = main %>% mutate({cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  adj3_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + Age + factor(Sex) + factor(RaceEthnicity4) + disadvantage2_13_17_qrtl + ComorbidityScore + factor(i2020), data = main %>% mutate({cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  
  # run models -----------
  glm_mod     <- eval(parse(text = glm_mod_text), envir = env_name)
  logistf_mod <- eval(parse(text = logistf_mod_text), envir = env_name)
  adj1_mod    <- eval(parse(text = adj1_mod_text), envir = env_name)
  adj2_mod    <- eval(parse(text = adj2_mod_text), envir = env_name)
  adj3_mod    <- eval(parse(text = adj3_mod_text), envir = env_name)
  
  # format output -----------
  unadjusted       <- tidy_model_output(mod = glm_mod, kable_digits = kable_digits)
  unadjusted_firth <- tidy_model_output(mod = logistf_mod, kable_digits = kable_digits)
  adjustment1      <- tidy_model_output(mod = adj1_mod, kable_digits = kable_digits)
  adjustment2      <- tidy_model_output(mod = adj2_mod, kable_digits = kable_digits)
  adjustment3      <- tidy_model_output(mod = adj3_mod, kable_digits = kable_digits)
  
  # results list ----------
  
  list(
    unadjusted       = unadjusted |> mutate(nobs = nobs(glm_mod)),
    unadjusted_firth = unadjusted_firth |> mutate(nobs = nobs(logistf_mod)),
    adjustment1      = adjustment1 |> mutate(nobs = nobs(adj1_mod)),
    adjustment2      = adjustment2 |> mutate(nobs = nobs(adj2_mod)),
    adjustment3      = adjustment3 |> mutate(nobs = nobs(adj3_mod))
  )
  
}
