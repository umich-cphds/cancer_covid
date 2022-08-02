type_cascade_analysis <- function(data, exposure, outcome, int = NULL, env_name = .GlobalEnv, kable_digits = NULL) {
  
  if (exposure %in% c("breast_cancer", "prostate_cancer", "breast_cancer_cat", "prostate_cancer_cat")) {
    cli::cli_alert_info("Not adjusting for sex")
    tmp_adj_sets <- lapply(adj_sets, \(x) x[!(x %in% c("factor(Sex)"))])
  } else {
    tmp_adj_sets <- adj_sets
  }
  
  for (i in seq_along(exposure)) {
    if (!grepl("factor\\(", exposure[i])) {
      exposure[i] <- paste0("factor(", exposure[i],")")
    }
  }
  
  if (length(exposure) > 1) {
    # exposure <- paste0("factor(", exposure, ")")
    exposure <- paste0(exposure, collapse = " + ")
  }

  
  # model setup ---------
  if (is.null(int)) {
    glm_mod_text     <- glue::glue("glm({outcome} ~ {exposure}, data = {data}, family = 'binomial')")
    logistf_mod_text <- glue::glue("logistf::logistf({outcome} ~ {exposure}, data = {data})")
    adj1_mod_text    <- glue::glue("logistf::logistf({outcome} ~ {exposure} + {paste(tmp_adj_sets[['adj1']], collapse = ' + ')}, data = {data}, control = logistf.control(maxit = 1000))")
    adj2_mod_text    <- glue::glue("logistf::logistf({outcome} ~ {exposure} + {paste(tmp_adj_sets[['adj2']], collapse = ' + ')}, data = {data}, control = logistf.control(maxit = 1000))")
    adj3_mod_text    <- glue::glue("logistf::logistf({outcome} ~ {exposure} + {paste(tmp_adj_sets[['adj3']], collapse = ' + ')}, data = {data}, control = logistf.control(maxit = 1000))")
  } else {
    glm_mod_text     <- glue::glue("glm({outcome} ~ {exposure}, data = {data}, family = 'binomial')")
    logistf_mod_text <- glue::glue("logistf::logistf({outcome} ~ {exposure}, data = {data})")
    adj1_mod_text    <- glue::glue("logistf::logistf({outcome} ~ {exposure} + factor({int}) + {exposure}:{int} + {paste(tmp_adj_sets[['adj1']], collapse = ' + ')}, data = {data}, control = logistf.control(maxit = 1000))")
    adj2_mod_text    <- glue::glue("logistf::logistf({outcome} ~ {exposure} + factor({int}) + {exposure}:{int} + {paste(tmp_adj_sets[['adj2']], collapse = ' + ')}, data = {data}, control = logistf.control(maxit = 1000))")
    adj3_mod_text    <- glue::glue("logistf::logistf({outcome} ~ {exposure} + factor({int}) + {exposure}:{int} + {paste(tmp_adj_sets[['adj3']], collapse = ' + ')}, data = {data}, control = logistf.control(maxit = 1000))")
  }
  
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