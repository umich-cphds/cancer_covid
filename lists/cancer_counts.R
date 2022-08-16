library(data.table)
library(openxlsx)

main <- readRDS("data/main_data.rds")
main_ids <- main[, unique(id)]

codes         <- readRDS("lists/cancer_codes.rds")
cancer_codes  <- codes$cancer_phecodes
cancer_xcodes <- paste0("X", codes$cancer_phecodes)

pheinfo <- get(load(file = "/net/junglebook/michiganmedicine/larsf/data/phenomes/Phecode_Definitions_1.2_Full.Rsav"))

pim <- fread("/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/20220202/phenomes/UNFILTERED_20220202/UNFILTERED_20220202_PEDMASTER_0.txt")
pim <- pim[IID %in% main_ids, ..cancer_xcodes]


positive_counts <- data.table(
  "phecode"  = cancer_codes,
  "counts" = t(pim[, lapply(.SD, \(x) sum(x, na.rm = TRUE))])[, 1]
  )


cancer_counts <- merge.data.table(
  pheinfo[phecode %in% cancer_codes][, .(phecode, phenotype)],
  positive_counts,
  by = "phecode"
  )

write.xlsx(x = cancer_counts, file = "objects/cancer_counts.xlsx", overwrite = TRUE)

