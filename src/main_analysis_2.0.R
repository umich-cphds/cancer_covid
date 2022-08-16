main_analysis_2.0 <- function(
    outcomes = c("`Severe COVID`", "Hospitalized", "ICU", "Deceased"),
    exposure,
    covariates = c("Age", "Sex", "RaceEthnicity4", "disadvantage2_13_17_qrtl", "ComorbidityScore"),
    interaction = NULL,
    model_term = NULL,
    dat) {
  
  if (is.null(model_term)) {
    model_term <- rep(NA, length(exposure))
    for (i in seq_along(exposure)) {
      if (!grepl("factor\\(", exposure[i])) {
        model_term[i] <- paste0("factor(", exposure[i], ")")
      } else {
        model_term[i] <- exposure[i]
      }
    }
    # model_term <- paste0("factor\\(", exposure_var, "\\)")
  }
  
  if (grepl("prostate", exposure) | grepl("breast", exposure)) {
    covariates <- covariates[!(tolower(covariates) == "sex")]
  }
  
  forms <- paste0(outcomes, " ~ ", exposure, " + ", paste0(covariates, collapse = " + "))
  if (!is.null(interaction)) {
    forms <- paste0(forms, paste0(" + factor(", interaction, ") + ", exposure, ":", interaction))
  }
  forms
  
  mods <- purrr::map(forms,
                     ~logistf(formula = .x, data = dat, control = logistf.control(maxit = 1000)))
  
  mods_out <- purrr::map(mods,
                         ~tidy_model_output(mod = .x) |>
                           as.data.table())
  names(mods_out) <- outcomes
  
  clean <- map2(mods_out, outcomes,
               \(x,y) {
                 tmp <- x[term != "(Intercept)"][, .(term, OR_print)]
                 names(tmp) <- c("term", y)
                 return(tmp)
               }) |>
    reduce(merge.data.table, by = "term")
  
  mods_out[["clean"]] <- clean
  
  return(mods_out)
  
}
