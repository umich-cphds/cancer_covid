## libraries
library(data.table)
library(ggplot2)
library(purrr)

## load results
tmp <- readRDS("../cancer_covid_revision/objects/any_cancer_full_results.rds")

## plot colors
canc_stat_cols <- c(
  "Cancer"    = "#FC8D62",
  "No cancer" = "#8DA0CB"
)

## load and prep plot data
plot_data <- rbindlist(
  purrr::map2(tmp, rep(c("Severe COVID", "Hospitalization", "ICU admission", "Mortality"), 2), ~as.data.table(.x)[term == "ComorbidityScore"][, outcome := .y]))[, outcome := factor(outcome, levels = c("Severe COVID", "Hospitalization", "ICU admission", "Mortality"))][]

## identify significant interactions
sig <- rbindlist(purrr::map2(tmp[1:4], c("Severe COVID", "Hospitalization", "ICU admission", "Mortality"), ~as.data.table(.x)[term == "AnyCancerPhe1:ComorbidityScore"][, outcome := .y][, .(outcome, ref, p_value)][, sig := fifelse(p_value < 0.05, 1, 0)]))

## plot
plot_data |>
  ggplot(aes(x = outcome, y = estimate, color = ref)) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray40", size = 1) +
  geom_pointrange(aes(ymin = conf_low, ymax = conf_high), size = 1, position = position_dodge(.2)) +
  geom_point(data = sig[sig == 1], aes(x = outcome, y = 0.54), color = "black", shape = 8) +
  scale_color_manual(values = canc_stat_cols) +
  labs(
    title = "Comorbidity score odds ratio by COVID-19 outcome",
    x = "Outcome",
    y = "OR (95% CI)",
    caption = "**Notes:**<br>
      - Error bars indicated 95% confidence interval<br>
      - '*' indicates p-value for interaction significant at 0.05 level<br>
      - Adjusted for age, sex, race/ethnicity, disadvantage index (quartile), and cancer status (ever/never)"
  ) +
  ylim(0.5, 2) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = ggtext::element_markdown(hjust = 0),
    legend.position = "top",
    legend.title = element_blank()
    )

## save
ggsave(filename = glue::glue("../cancer_covid_revision/objects/comorbid_score_plot.pdf"), width = 8, height = 5.5)

# ##### OLD ###########
# # for counting sample sizes
# tot <- na.omit(main, c("Age", "Sex", "RaceEthnicity4", "disadvantage2_13_17_qrtl", "ComorbidityScore", "AnyCancerPhe"))
# 
# tot[, table(AnyCancerPhe)]
# tot[, table(yscancer_category)]
# 
# tot[, table(cancer_treatment)]
# tot[, table(chemo_strat_treatment)]
# 
# tot[, table(bladder_cancer_cat)]
# tot[, table(breast_cancer_cat)]
# tot[, table(colorectal_cancer_cat)]
# tot[, table(lymphoid_cat)]
# tot[, table(myeloid_cat)]
# tot[, table(kidney_cancer_cat)]
# tot[, table(lung_cancer_cat)]
# tot[, table(skin_cancer_cat)]
# tot[, table(prostate_cancer_cat)]
