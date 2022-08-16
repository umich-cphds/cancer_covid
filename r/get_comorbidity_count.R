# top matter
source("libraries.R")
purrr::walk(list.files("src/"), ~source(paste0("src/", .x)))

chrt <- "20220801"

# load file paths
file_paths <- get_file_paths(cohort = chrt)

# load comorbidity codes
comorbidities <- readRDS("lists/comorbidities.rds")

comorbidities <- purrr::map_dfr(names(comorbidities),
           ~data.table(
             class = rep(.x, length(comorbidities[[.x]])),
             phecode = comorbidities[[.x]]
           ))

# load cohort data
whole <- readRDS(paste0("data/whole_data_", chrt,".rds"))

# load phecode data
icd_phecode <- fst::read_fst(paste0("data/", chrt, "/processed_phecode_data_", chrt, ".fst"), as.data.table = TRUE)

# subset to those in cohort and with comorbidity codes
comorbid_phecode <- icd_phecode[id %in% whole[, id]][phecode %in% comorbidities[, phecode]]

# merge key date into phecode data
merged <- merge.data.table(
  comorbid_phecode[, .(id, phecode, dsb = DaysSinceBirth)],
  whole[, .(id, key_date)],
  by = c("id"),
  all.x = TRUE
)

# subset phecode data to before key date
sub_merged <- unique(merged[dsb < key_date][, .(id, phecode)])

# count number of each phecode
n_comorbidities <- sub_merged[, .N, by = phecode]

n_comorbidities <- merge.data.table(
  comorbidities,
  n_comorbidities,
  by = "phecode"
)

# attach descriptions
pheinfo <- get(load(file = "/net/junglebook/michiganmedicine/larsf/data/phenomes/Phecode_Definitions_1.2_Full.Rsav"))

out <- merge.data.table(
  n_comorbidities,
  pheinfo[, .(phecode, description = phenotype)],
  all.x = TRUE,
  by = "phecode"
)[, .(class, phecode, description, count = N)][order(class, phecode)][, class := stringr::str_to_title(class)][]

# save
fwrite(out, paste0("objects/", chrt,"/comorbidities_count.csv"))
