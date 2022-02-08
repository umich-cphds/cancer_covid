interaction_model <- function(dataset, exposure, outcome, covariates, env_name = .GlobalEnv, int = NULL, kable_digits = NULL) {
 
  tmp_covariates <- covariates[!covariates == exposure]
  
  if (exposure %in% c("RespiratoryDiseases", "CirculatoryDiseases", "Type2Diabetes",
                      "KidneyDiseases", "LiverDiseases", "AutoimmuneDiseases")) {
    
    if (is.null(int)) {
      mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')}, data = {dataset} %>% mutate(ComorbidityScore = ComorbidityScore - as.numeric(as.character({exposure}))))")
    } else {
      mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')} + {int}:{exposure}, data = {dataset} %>% mutate(ComorbidityScore = ComorbidityScore - as.numeric(as.character({exposure}))))")
    }
    
  } else {
    
    if (is.null(int)) {
      mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')}, data = {dataset})")
    } else {
      mod_text <- glue::glue("logistf({outcome} ~ {exposure} + {paste(tmp_covariates, collapse = ' + ')} + {int}:{exposure}, data = {dataset})")
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