# create workbook of results
results_to_workbook <- function(results, filename = NULL) {
  
  if (is.null(filename)) {
    filename = deparse(substitute(results))
  }
  
  wb <- createWorkbook()
  
  terms <- unique(results$clean$term)
  terms <- terms[!grepl("\\:", terms)]
  
  for (i in seq_along(terms)) {
    addWorksheet(wb, sheetName = gsub("/", "_", terms[i]))
    writeData(wb, sheet = i, x = results$clean %>% filter(term == terms[i]))
  }
  
  saveWorkbook(wb, paste0("objects/", filename, ".xlsx"), overwrite = TRUE)
  
}
