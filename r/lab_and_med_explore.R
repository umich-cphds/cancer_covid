### exploring laboratory and administered medication data for use in
### cancer-covid paper revision
library(data.table)

### file paths
# labs_path <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883/20220701/LabResults_2022-07-02.txt"
meds_path <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883/20220701/MedAdministrations_2022-07-02.txt"

### load data
# labs <- fread(labs_path)
meds <- fread(meds_path)

### functions

## function will take a brand or generic drug name (`drug_name`) and search
## the administered medication data and extract
## GUID (globally unique identifier)
extract_med_guid <- function(data, drug_name, medname_var = "MedicationName", guid_var = "MedicationGUID") {
  
  cli::cli_alert_info("pulling guids for {drug_name}")
  as.character(unique(data[grepl(drug_name, tolower(data[[medname_var]])), ][[guid_var]]))
  
}

### drugs
## lists of brand and generic drug names for different classes of interest

# anti-CD20 antibody drugs
antiCD20drugs <- c("rituximab", "obinutuzumab", "ofatumumab", "ublituximab", "veltuzumab")

# BTK inhibitors
btk_drugs <- tolower(c("Ibrutinib", "Acalabrutinib", "LOXO-305"))

# Granulocyte-colony stimulating factor
gcsf_drugs <- tolower(c("Neupogen", "Zarxio", "Nivestym", "filgrastim"))

### extract GUIDs for drugs
antiCD20_guids <- purrr::map(antiCD20drugs,
                            ~extract_med_guid(data = meds, drug_name = .x))
btk_guids <- purrr::map(btk_drugs,
                        ~extract_med_guid(data = meds, drug_name = .x))
gcsf_guids <- purrr::map(gcsf_drugs,
                        ~extract_med_guid(data = meds, drug_name = .x))

any(grepl("rituximab", tolower(meds[, MedicationName])))
rit <- meds[grepl("rituximab", tolower(meds[, MedicationName])), ]


rit_guids <- unique(rit[, MedicationGUID])

### extract unique encrypted ids for each class of drugs
btk_ids      <- unique(meds[tolower(MedicationGUID) %in% unique(unlist(btk_guids)), Encrypted_PatientID])
antiCD20_ids <- unique(meds[tolower(MedicationGUID) %in% unique(unlist(antiCD20_guids)), Encrypted_PatientID])
gcsf_ids     <- unique(meds[tolower(MedicationGUID) %in% unique(unlist(gcsf_guids)), Encrypted_PatientID])

stem_cell <- unique(procedures[ProcedureCode %in% c("38240", "38241", "Z94.84")][, Encrypted_PatientID])

### counts of individuals receiving each class of drug
purrr::map(list(btk_ids, antiCD20_ids, gcsf_ids), ~length(unique(.x)))
