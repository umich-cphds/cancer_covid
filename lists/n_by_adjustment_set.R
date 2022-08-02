tmp_adj_sets <- lapply(adj_sets, \(x) gsub("\\)", "", gsub("factor\\(", "", x)))

n_total <- na.omit(main, cols = tmp_adj_sets[["adj1"]])
n_adj2  <- na.omit(main, cols = tmp_adj_sets[["adj2"]])
n_adj3  <- na.omit(main, cols = tmp_adj_sets[["adj3"]])

extract <- function(x) {
data.table(
  "type" = c(
    "n_total",
    "n_severe", "n_hosp", "n_icu", "n_death",
    "n_chemo", "n_rad_only", "n_surg_only",
    "n_any", "n_skin", "n_heme", "n_breast", "n_prostate", "n_lung",
    "nr_chemo", "nr_rad_only", "nr_surg_only",
    "nr_any", "nr_skin", "nr_heme", "nr_breast", "nr_prostate", "nr_lung"),
  "n" = c(
    x[, .N],
    x[`Severe COVID` == 1, .N], x[Hospitalized == 1, .N], x[ICU == 1, .N], x[Deceased == 1, .N],
    x[chemo == 1, .N], x[rad_only == 1, .N], x[surgery_only == 1, .N],
    x[AnyCancerPhe == 1, .N], x[skin_cancer == 1, .N], x[heme_malign == 1, .N], x[breast_cancer == 1, .N], x[prostate_cancer == 1, .N], x[lung_cancer == 1, .N],
    x[recent_chemo == 1, .N], x[recent_rad_only == 1, .N], x[recent_surgery_only == 1, .N],
    x[recent_AnyCancerPhe == 1, .N], x[recent_skin_cancer == 1, .N], x[recent_heme_malign == 1, .N], x[recent_breast_cancer == 1, .N], x[recent_prostate_cancer == 1, .N], x[recent_lung_cancer == 1, .N]
  ),
  "category" = c("total", rep("outcome", 4), rep("treatment", 3), rep("type", 6), rep("recent_treatment", 3), rep("recent_type", 6))
  )
}

tmp1 <- extract(x = n_total)
tmp2 <- extract(x = n_adj2)
tmp3 <- extract(x = n_adj3)

simple <- merge(merge(
  tmp1[, .(order = 1:23, type, category, `AdjSet1` = n)],
  tmp2[, .(type, `AdjSet2` = n)],
  by = "type"
),
tmp3[, .(type, `AdjSet3` = n)],
by = "type")[order(order)][, !c("order")]

write.xlsx(simple, file = "objects/n_by_adjustment_set.xlsx", overwrite = TRUE)