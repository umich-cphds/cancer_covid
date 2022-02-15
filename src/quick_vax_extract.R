quick_vax_extract <- function(x) {
  
  tmp <- x$adjustment3$term
  tmp <- tmp[grepl("factor\\(vax_status\\)*", tmp) & !grepl("\\:", tmp)]
  
  out <- deparse(substitute(x))
  
  taco <- x$adjustment3 %>%
    filter(term %in% tmp) %>%
    mutate(model = out) %>%
    select(model, everything())
  
  tmp_taco <- taco %>%
    mutate(term = gsub("factor\\(vax_status\\)", "", term)) %>%
    select(term, tmp = OR_print)
  names(tmp_taco)[names(tmp_taco) == "tmp"] <- out
  
  list(
    "full" = taco,
    "tidy" = tmp_taco
  )
  
  
}
