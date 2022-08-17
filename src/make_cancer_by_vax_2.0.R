extract_vax_and_assign <- function(cancer_by_vax_res, var, outcome) {
  
  purrr::map_dfr(names(cancer_by_vax_res),
                 ~cancer_by_vax_res[[.x]][term == var, .(term, estimate, conf_low, conf_high)][, `:=` (vax_status = .x, outcome = outcome)][])
  
}

cancer_by_vax_2.0 <- function(
    outcome = "`Severe COVID`",
    exposure = "vax_status",
    covariates = c("Age", "Sex", "RaceEthnicity4", "disadvantage2_13_17_qrtl", "ComorbidityScore"),
    int_var = "AnyCancerPhe",
    int_ref_level = "0",
    ref_var = "AnyCancerPhe1",
    dat = "main") {
  
  mods <- map(eval(parse(text = paste0("levels(", dat, "[['", exposure, "']])"))),
              ~paste0("logistf(", outcome, " ~ ", exposure, " + ", int_var, " + ", exposure, ":", int_var, " + ", paste0(covariates, collapse = " + "), ", data = ", dat, " |> mutate(", int_var, " = relevel(", int_var, ", ref = '", int_ref_level, "'), ", exposure, " = relevel(", exposure, ", ref = '", .x, "')), control = logistf.control(maxit = 1000))")) |>
    map(~eval(parse(text = .x))) |>
    map(~tidy_model_output(.x) |> as.data.table())
  names(mods) <- eval(parse(text = paste0("levels(", dat, "[['", exposure, "']])")))
  
  extract <- extract_vax_and_assign(
    cancer_by_vax_res = mods,
    var = ref_var,
    outcome = gsub("`", "", outcome)
  )
  
  mods[["clean"]] <- extract
  
  return(mods)
  
}
