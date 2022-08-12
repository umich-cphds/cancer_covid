### exploring laboratory and administered medication data for use in
### cancer-covid paper revision
library(data.table)

### file paths
meds_path <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883/20220701/MedAdministrations_2022-07-02.txt"
cancer_treatment_id_table_path <- "lists/cancer_treatment_guid_table.csv"

### load data
meds <- fread(meds_path)
cancer_treatment_id_table <- fread(cancer_treatment_id_table_path)

# Exclude entries that likely indicate that medication was not administered
meds <- meds[!MedicationStatus %in% 
               c("MAR Hold", "Held by Provider", "Paused", "Stopped",
                 "Completed", "Missed", "See Alternative", "Canceled Entry",
                 "Not Given","Refused"), ]

# Subset to entries in cancer_treatment_table
meds <- meds[MedicationGUID %in% cancer_treatment_id_table[, id]]

# Subset by therapy class
cyto <- meds[MedicationGUID %in% cancer_treatment_id_table[class == "cytotoxic chemo", id]]
immu <- meds[MedicationGUID %in% cancer_treatment_id_table[class == "immunotherapy", id]]
horm <- meds[MedicationGUID %in% cancer_treatment_id_table[class == "hormone therapy", id]]
targ <- meds[MedicationGUID %in% cancer_treatment_id_table[class == "targeted therapy", id]]

# select first and last observation by person
first_last <- function(dat, class) {
  
  first <- dat[ dat[order(Encrypted_PatientID, DoseStartTime_DaysSinceBirth), .I[1], by = Encrypted_PatientID]$V1 ]
  last  <- dat[ dat[order(Encrypted_PatientID, DoseStartTime_DaysSinceBirth), .I[.N], by = Encrypted_PatientID]$V1 ]
  out <- merge.data.table(
    first[, .(id = Encrypted_PatientID, min_dsb = DoseStartTime_DaysSinceBirth)],
    last[, .(id = Encrypted_PatientID, max_dsb = DoseStartTime_DaysSinceBirth)]
  )
  
  setnames(out, old = c("min_dsb", "max_dsb"), new = c(paste0(class, "_min_dsb"), paste0(class, "_max_dsb")))
  
  return(out)

}

# extract first and last dsb for each therapy
cyto_first_last <- first_last(dat = cyto, class = "cyto")
immu_first_last <- first_last(dat = immu, class = "immu")
horm_first_last <- first_last(dat = horm, class = "horm")
targ_first_last <- first_last(dat = targ, class = "targ")

# merge
comb <- merge.data.table(
  cyto_first_last,
  immu_first_last,
  by = "id",
  all = TRUE
)
comb <- merge.data.table(
  comb,
  horm_first_last,
  by = "id",
  all = TRUE
)
comb <- merge.data.table(
  comb,
  targ_first_last,
  by = "id",
  all = TRUE
)

# create indicators for each class of anticancer therapy
comb[, `:=` (
  cyto_ever = fifelse(id %in% cyto_first_last[, id], 1, 0),
  immu_ever = fifelse(id %in% immu_first_last[, id], 1, 0),
  horm_ever = fifelse(id %in% horm_first_last[, id], 1, 0),
  targ_ever = fifelse(id %in% targ_first_last[, id], 1, 0)
)]

# save
fwrite(comb, "objects/processed_cancer_therapies.csv")
