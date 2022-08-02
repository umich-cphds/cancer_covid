# print tidy model output ----------
tidy_model_output <- function(mod, kable_digits = NULL) {
  
  out <- broomExtra::tidy_parameters(mod, df_method = "wald") %>%
    dplyr::mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) %>%
    dplyr::select(term, estimate, conf_low = conf.low, conf_high = conf.high, p_value = p.value) %>%
    dplyr::mutate(
      OR_print = paste0(
        sprintf("%.2f", round(estimate, 2)),
        " (",
        sprintf("%.2f", round(conf_low, 2)),
        ", ",
        sprintf("%.2f", round(conf_high, 2)), ")")) %>%
    dplyr::relocate(term, estimate, conf_low, conf_high, OR_print, p_value)
  
  if (!is.null(kable_digits)) {
    out <- out %>% kable(digits = kable_digits)
  }
  
  return(out)
  
}