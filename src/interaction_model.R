interaction_model <- function(dataset, exposure, outcome, covariates, env_name = .GlobalEnv, int = NULL, kable_digits = NULL, ref = "0") {
 
  tmp_covariates <- covariates[covariates %notin% c(exposure)]
  
  if (exposure %in% c("RespiratoryDiseases", "CirculatoryDiseases", "Type2Diabetes",
                      "KidneyDiseases", "LiverDiseases", "AutoimmuneDiseases")) {
    
    if (is.null(int)) {
      mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')}, data = {dataset} %>% mutate(ComorbidityScore = ComorbidityScore - as.numeric(as.character({exposure})), {int} = relevel(factor({int}), ref = '{ref}')), control = logistf.control(maxit = 1000))")
    } else {
      mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')} + {int}:{exposure}, data = {dataset} %>% mutate(ComorbidityScore = ComorbidityScore - as.numeric(as.character({exposure})), {int} = relevel(factor({int}), ref = '{ref}')), control = logistf.control(maxit = 1000))")
    }
    
  } else {
    
    if (is.null(int)) {
      mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')}, data = {dataset} %>% mutate({int} = relevel(factor({int}), ref = '{ref}')), control = logistf.control(maxit = 1000))")
    } else {
      mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')} + {int}:{exposure}, data = {dataset} %>% mutate({int} = relevel(factor({int}), ref = '{ref}')), control = logistf.control(maxit = 1000))")
    }
    
  }
  
  tmp_mod <- eval(parse(text = mod_text), envir = env_name)
  
  tidy_out <- tidy_model_output(mod = tmp_mod, kable_digits = kable_digits)
  
  list(
    model       = mod_text,
    tidy_output = tidy_out,
    nobs        = nobs(tmp_mod)
  )
  
   
}