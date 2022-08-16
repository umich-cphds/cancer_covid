## top matter
source("libraries.R")
library(fst)
purrr::walk(list.files("src/"), ~source(paste0("src/", .x)))

## load cancer phecodes
source("lists/cancer_codes.R")

chrt <- "20220801"

## load icd phecode data
icd_phecode <- fst::read_fst(paste0("data/", chrt, "/processed_phecode_data_", chrt, ".fst"), as.data.table = TRUE)
icd_phecode <- icd_phecode[phecode %in% codes$cancer_phecodes]
pheinfo <- get(load(file = "/net/junglebook/michiganmedicine/larsf/data/phenomes/Phecode_Definitions_1.2_Full.Rsav"))

primary_cancer_check <- function(
    cancer_name  = "colorectal cancer",
    target_codes = c("153", "153.2", "153.3"),
    nonspecific  = c("195", "195.1", "199", "158"),
    chrt         = "20220701",
    phecode_dat  = icd_phecode,
    na_vars      = c("Age", "Sex", "RaceEthnicity4", "disadvantage2_13_17_qrtl", "ComorbidityScore")
    ) {
  
  ## load file paths
  file_paths <- get_file_paths(cohort = chrt)
  
  
  ## load cohort data
  main <- readRDS(paste0("data/main_data_", chrt, ".rds"))
  
  # subset data to those with non-missing covariates
  if (!is.null(na_vars)) {
    main <- na.omit(main, na_vars)
  }
  
  # all non-colorectal cancer phecodes
  other_cancer <- codes$cancer_phecodes[!(codes$cancer_phecodes %in% c(target_codes, nonspecific))]
  
  ## subset phecode data to those in analysis
  sub_phecode <- phecode_dat[id %in% main[, id]]
  
  ## subset to first colorectal occurrence
  target_dat <- sub_phecode[phecode %in% target_codes]
  target_dat <- target_dat[
    target_dat[, .I[which.min(DaysSinceBirth)], by = id]$V1
  ][, .(id, target_dsb = DaysSinceBirth)]
  
  
  ## subset to first cancer occurrence
  first_cancer <- sub_phecode[phecode %in% other_cancer]
  first_cancer <- first_cancer[
    first_cancer[ , .I[which.min(DaysSinceBirth)], by = id]$V1
  ][, .(id, cancer_dsb = DaysSinceBirth, cancer_phe = phecode)]
  
  
  ## merge
  merged <- merge.data.table(
    target_dat,
    first_cancer,
    by = "id",
    all.x = T
  )[, target_first := fifelse(target_dsb <= cancer_dsb, "Yes", "No")]
  
  return(
    list(
      common_priors = merge.data.table(
        merged[target_first == "No", .N, cancer_phe],
        pheinfo[, .(cancer_phe = phecode, description = phenotype)],
        by = "cancer_phe", all.x = TRUE)[order(-N)],
      count = merged[, .N, by = target_first],
      prop  = merged[target_first == "No", .N] / merged[, .N],
      time  = summary(merged[target_first == "No"][, diff := target_dsb - cancer_dsb][, diff/365.22]),
      summary = paste0("Of the ", length(unique(target_dat[, id])), " ", cancer_name,
                       " cases identified, ",
                       round(merged[target_first == 'No', .N] * 100 / length(unique(target_dat[, id])), 1),
                       "% (n = ", merged[target_first == 'No', .N],
                       ") have another, specific cancer code prior to the initial ",
                       cancer_name,
                       " diagnosis. Of those with a prior, specific cancer code, the median time between first cancer diagnosis and ",
                       cancer_name,
                       " diagnosis was ",
                       round(median(merged[target_first == 'No'][, diff := (target_dsb - cancer_dsb)/365.22][, diff]), 1),
                       " years."
                       )
      )
    )
  
}

cancers <- c("bladder cancer", "breast cancer", "colorectal cancer", "hematologic malignancies", "lympoid malignancies", "myeloid malignancies", "kidney cancer", "lung cancer", "melanoma", "prostate cancer")
cancer_codes <- list(codes$bladder_cancer_phecodes, codes$breast_cancer_phecodes, codes$colorectal_cancer_phecodes, c(unlist(codes$heme_malign)), codes$heme_malign$lymphoid, codes$heme_malign$myeloid, codes$kidney_cancer_phecodes, codes$lung_cancer_phecodes, codes$skin_cancer_phecodes, codes$prostate_cancer_phecodes)

check <- map2(cancers, cancer_codes,
     ~primary_cancer_check(cancer_name = .x, target_codes = .y))
names(check) <- cancers

saveRDS(check, paste0("objects/primary_cancer_check_", chrt, ".rds"))

primary_prop_table <- imap_dfr(cancers,
        ~data.table(cancer = cancers[.y], prop_not_primary = check[[cancers[.y]]][["prop"]]))[order(-prop_not_primary)]

fwrite(primary_prop_table, paste0("objects/primary_prop_table_", chrt, ".csv"))
