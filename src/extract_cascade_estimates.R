extract_cascade_estimates <- function(results, outcome, mod_term = "factor(AnyCancerPhe)1") {
  
  f         <- names(results)
  # tmp_terms <- c(mod_term, paste0(mod_term, "*"))
  
  
  for (i in seq_along(f)) {
    
    if (i == 1) {
      tmp_out <- results[[f[i]]] %>%
        # dplyr::filter(term %in% tmp_terms) %>%
        dplyr::filter(term %in% term[grepl(paste0(mod_term, "*", collapse = "|"), term)]) %>%
        mutate(model = f[i]) %>%
        dplyr::select(model, term, estimate, conf_low, conf_high, OR_print, p_value, nobs)
    } else {
      tmp_out <- bind_rows(tmp_out,
                           results[[f[i]]] %>%
                             # dplyr::filter(term %in% tmp_terms) %>%
                             dplyr::filter(term %in% term[grepl(paste0(mod_term, "*", collapse = "|"), term)]) %>%
                             mutate(model = f[i]) %>%
                             dplyr::select(model, term, estimate, conf_low, conf_high, OR_print, p_value, nobs))
    }
    
  }
  
  exclude <- c("factor(Sex)F", "factor(RaceEthnicity4)African American / Non-Hispanic", "factor(RaceEthnicity4)Other/Unknown")
  
  tidy_out <- tmp_out %>% filter(!(term %in% exclude)) %>%  select(model, term, OR_print)
  names(tidy_out)[names(tidy_out) == "OR_print"] <- outcome
  
  list(
    full = tmp_out,
    tidy = tidy_out
  )
  
}