extract_results  <- function(res_list, result, var, terms, recent = FALSE, cancer_ref = FALSE, man_int = NULL) {
  
  tmp_terms     <- terms
  if (cancer_ref == FALSE) {
    tmp_names <- c("term", "no_cancer_est", "no_cancer_low", "no_cancer_high", "no_cancer_print", "no_cancer_p_value", "p_int_value")
    if (recent == FALSE) {
      if (is.null(man_int)) {
        tmp_int_terms <- paste0(tmp_terms, ":AnyCancerPhe1") 
      } else {
          tmp_int_terms <- paste0(tmp_terms, ":", man_int)
        }
    } else {
      if (is.null(man_int)) {
        tmp_int_terms <- paste0(tmp_terms, ":recent_AnyCancerPhe1") 
      } else {
        tmp_int_terms <- paste0(tmp_terms, ":", man_int)
      }
    }
  }
  if (cancer_ref == TRUE) {
    tmp_names <- c("term", "cancer_est", "cancer_low", "cancer_high", "cancer_print", "cancer_p_value", "p_int_value")
    if (recent == FALSE) {
      if (is.null(man_int)) {
        tmp_int_terms <- paste0(tmp_terms, ":AnyCancerPhe0") 
      } else {
        tmp_int_terms <- paste0(tmp_terms, ":", man_int)
      }
    } else {
      if (is.null(man_int)) {
        tmp_int_terms <- paste0(tmp_terms, ":recent_AnyCancerPhe0") 
      } else {
        tmp_int_terms <- paste0(tmp_terms, ":", man_int)
      }
    }
  }
  
  tmp <- res_list[[result]][[var]]$tidy_output %>%
    dplyr::filter(term %in% tmp_terms)
  
  tmp_int <- tibble(
    term        = tmp_terms,
    p_int_value = res_list[[result]][[var]]$tidy_output %>%
      dplyr::filter(term %in% tmp_int_terms) %>%
      dplyr::pull(p_value)
  )
  
  if (any(tmp_int$p_int_value < 0.01)) {
    tmp_int <- tmp_int %>%
      dplyr::mutate(
        p_int_value = case_when(
          p_int_value < 0.01 ~ "<0.01",
          T ~ format(round(p_int_value, 2), nsmall = 2)
        )
      )
  } else {
    tmp_int <- tmp_int %>%
      dplyr::mutate(p_int_value = format(round(p_int_value, 2), nsmall = 2))
  }
  if (any(tmp$p_value < 0.01)) {
    tmp <- tmp %>%
      dplyr::mutate(
        p_value = case_when(
          p_value < 0.01 ~ "<0.01",
          T ~ format(round(p_value, 2), nsmall = 2)
        )
      )
  } else {
    tmp <- tmp %>%
      dplyr::mutate(p_value = format(round(p_value, 2), nsmall = 2))
  }
  
  tmp_out <- tmp %>% left_join(tmp_int, by = "term")
  names(tmp_out) <- tmp_names
  
  return(tmp_out)
  
}
