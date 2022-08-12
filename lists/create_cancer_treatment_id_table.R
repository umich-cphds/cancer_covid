library(data.table)

# read in list of curated anticancer drugs/therapies and their classes
ct <- fread("lists/anticancer_treatment_names.csv")
ct[, table(class)]

# read in medication-ATC mapping table
med_table <- fread("/net/junglebook/michiganmedicine/larsf/Projects/Medications/data/MM_medmap_RXNORM_ATC.txt")

# subset to ATC class L: Antineoplastic and immunomodulating agents
antineo <- med_table[grepl("L", ATC), ]

# create function for extracting GUIDs from list of anticancer drug/therapy names
extract_med_guid <- function(med_name, med_tab) {
  as.character(med_tab[grepl(med_name, tolower(MedicationName)) & grepl("L", ATC), ][, unique(MedicationGUID)])
}

# create subtables corresponding to each anticancer drug/therapy class
cyto <- ct[class == "chemotherapy"]
immu <- ct[class == "immunotherapy"]
horm <- ct[class == "hormone therapy"]
targ <- ct[class == "targeted therapy"]

# pull GUIDs corresponding to medications in each anticancer drug/therapy class
cyto_guids <- unique(unlist(purrr::map(cyto[, name],
                         ~extract_med_guid(med_name = .x, med_tab = antineo))))

immu_guids <- unique(unlist(purrr::map(immu[, name],
                                       ~extract_med_guid(med_name = .x, med_tab = antineo))))

horm_guids <- unique(unlist(purrr::map(horm[, name],
                                       ~extract_med_guid(med_name = .x, med_tab = antineo))))

targ_guids <- unique(unlist(purrr::map(targ[, name],
                                       ~extract_med_guid(med_name = .x, med_tab = antineo))))

# create a single table
cancer_treatment_id_table <- rbindlist(list(
  data.table(class = rep("cytotoxic chemo", length(cyto_guids)), id = cyto_guids),
  data.table(class = rep("immunotherapy", length(immu_guids)), id = immu_guids),
  data.table(class = rep("hormone therapy", length(horm_guids)), id = horm_guids),
  data.table(class = rep("targeted therapy", length(targ_guids)), id = targ_guids)
))

cancer_treatment_id_table <- cancer_treatment_id_table[!(id == "190025" & class == "hormone therapy")]

cancer_treatment_id_table

fwrite(cancer_treatment_id_table, "lists/cancer_treatment_guid_table.csv")

# antineo[MedicationGUID %in% as.integer(cyto_guids)][ATC_Level == 3][, table(ATC)]
# 
# antineo[grepl("bevacizumab", tolower(MedicationName))]
# 
# cancer_treatment_ids <- list(
#   "cytotoxic_chemo"  = cyto_guids,
#   "immunotherapy"    = immu_guids,
#   "hormone_therapy"  = horm_guids[!horm_guids %in% c("190025")],
#   "targeted_therapy" = targ_guids
# )
# 
# 
# 
# intersect(horm_guids, immu_guids)
# 
# names(cyto_guids) <- cyto[, name]
# 
# 
# med_table[ATC == "L01D"]
# 
# med_table[grepl("paclitaxel", tolower(MedicationName)) & grepl("L", ATC), ]
# 
# antineo_pheno <- med_pheno[grepl("L01D", ATC), ]
