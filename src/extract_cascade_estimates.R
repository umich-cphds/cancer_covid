extract_cascade_estimates <- function(results, outcome, mod_term = "factor(AnyCancerPhe)1") {
  
  f <- names(results)
  
  for (i in seq_along(f)) {
    
    if (i == 1) {
      tmp_out <- results[[f[i]]] %>%
        dplyr::filter(term == mod_term) %>%
        mutate(model = f[i]) %>%
        dplyr::select(model, term, estimate, conf_low, conf_high, OR_print, p_value, nobs)
    } else {
      tmp_out <- bind_rows(tmp_out,
                           results[[f[i]]] %>%
                             dplyr::filter(term == mod_term) %>%
                             mutate(model = f[i]) %>%
                             dplyr::select(model, term, estimate, conf_low, conf_high, OR_print, p_value, nobs))
    }
    
  }
  
  tidy_out <- tmp_out %>% select(model, OR_print)
  names(tidy_out)[names(tidy_out) == "OR_print"] <- outcome
  
  list(
    full = tmp_out,
    tidy = tidy_out
  )
  
}