library(data.table)
library(ggplot2)

cp <- readRDS("../cancer_covid_revision/objects/primary_cancer_check_20220701.rds")
d <- fread("../cancer_covid_revision/objects/primary_prop_table_20220701.csv")
d[, `:=` (
  cancer = stringr::str_to_title(cancer),
  colo_ind = fifelse(cancer == "colorectal cancer", "yes", "no")
  )]

cols <- c(
  "yes" = "skyblue",
  "no" = "gray40"
)


d |>
  ggplot(aes(x = reorder(cancer, prop_not_primary), y = prop_not_primary)) +
  geom_bar(aes(fill = colo_ind), stat = "identity") +
  geom_text(aes(label = round(prop_not_primary, 2)), hjust = 1.2, fontface = "bold", color = "white") +
  labs(
    title = "Proportion with prior, specific cancer diagnosis by cancer site",
    x = "",
    y = "Proportion with prior, specific cancer diagnosis"
  ) +
  scale_fill_manual(values = cols) +
  ylim(c(0, .6)) +
  # scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    legend.position = "none"
  )
ggsave("plots/prior_cancer_diagnosis.pdf", width = 8, height = 5, device = cairo_pdf)

fwrite(cp[["colorectal cancer"]][["common_priors"]], "../cancer_covid_revision/objects/common_colorectal_priors.csv")
