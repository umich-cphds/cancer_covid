## libraries
library(data.table)
library(purrr)
library(ggplot2)
library(ggtext)

## load data
# load no cancer-reference data
tmp <- purrr::map2(c("sc", "hosp", "icu", "death"), c("Severe COVID", "Hospitalization", "ICU Admission", "Deceased"),
           ~fread(paste0("../cancer_covid_revision/objects/vax_ev_", .x, ".csv"))[, `:=` (outcome = .y, ref = "No cancer")])
# load cancer-reference data
tmp_cr <- purrr::map2(c("sc_cr", "hosp_cr", "icu_cr", "death_cr"), c("Severe COVID", "Hospitalization", "ICU Admission", "Deceased"),
                   ~fread(paste0("../cancer_covid_revision/objects/vax_ev_", .x, ".csv"))[, `:=` (outcome = .y, ref = "Cancer")])

# identify significant interactions
sig <- rbindlist(purrr::map(tmp, ~.x[term == "vax_ever:factor(AnyCancerPhe)1"][, .(outcome, ref, p_value)][, sig := fifelse(p_value < 0.05, 1, 0)]))[outcome == "Deceased", outcome := "Mortality"]

# colors for plot
canc_stat_cols <- c(
  "Cancer"    = "#FC8D62",
  "No cancer" = "#8DA0CB"
)

# plot
rbindlist(list(
  purrr::map_dfr(tmp, ~.x[term == "vax_ever"]),
  purrr::map_dfr(tmp_cr, ~.x[term == "vax_ever"])
))|>
  DT(outcome == "ICU Admission", outcome := "ICU admission") |>
  DT(outcome == "Deceased", outcome := "Mortality") |>
  DT(, outcome := factor(outcome, levels = c("Severe COVID", "Hospitalization", "ICU admission", "Mortality"))) |>
  ggplot(aes(x = outcome, y = estimate, color = ref)) +
  geom_hline(yintercept = 1, size = 1, color = "gray40", linetype = 2) +
  geom_pointrange(aes(ymin = conf_low, ymax = conf_high), size = 1, position = position_dodge(.2)) +
  geom_point(data = sig[sig == 1], aes(x = outcome, y = 0.04), color = "black", shape = 8) +
  ylim(0, 1) +
  labs(
    title = "ORs for COVID-19 outcomes corresponding to vaccination status\nstratified by cancer status",
    subtitle = "Ever vs never (reference)",
    x = "Outcome",
    y = "Vaccination status (ever) odds ratio (95% CI)",
    caption = "**Notes:**<br>
      - Error bars indicated 95% confidence interval<br>
      - '*' indicates p-value for interaction significant at 0.05 level"
  ) + 
  scale_color_manual(values = canc_stat_cols) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_markdown(hjust = 0)
  )

# save
ggsave(filename = glue::glue("../cancer_covid_revision/objects/vax_ev_int_plot.pdf"), width = 8, height = 5.5)

### OLD ########
# purrr::map(tmp,
#            ~.x[term == "factor(AnyCancerPhe)1"]) |>
#   rbindlist()|>
#   DT(, outcome := factor(outcome, levels = c("Severe COVID", "Hospitalization", "ICU Admission", "Deceased"))) |>
#   ggplot(aes(x = outcome, y = estimate, color = outcome)) +
#   geom_hline(yintercept = 1, size = 1, color = "gray40", linetype = 2) +
#   geom_errorbar(aes(ymin = conf_low, ymax = conf_high), size = 1, width = 0.2) +
#   geom_point(size = 3) +
#   ylim(0, NA) +
#   labs(
#     title = "Any cancer odds ratio for COVID-19 outcome",
#     subtitle = "Yes vs no (reference)",
#     x = "Outcome", y = "Odds ratio (95% confidence interval)"
#   ) +
#   scale_color_brewer(palette = "Set2") +
#   theme_classic() +
#   theme(
#     plot.title = element_text(face = "bold"),
#     legend.position = "none"
#   )
# 
# purrr::map(tmp,
#            ~.x[term == "vax_ever:factor(AnyCancerPhe)1"]) |>
#   rbindlist()|>
#   DT(, outcome := factor(outcome, levels = c("Severe COVID", "Hospitalization", "ICU Admission", "Deceased"))) |>
#   ggplot(aes(x = outcome, y = estimate, color = outcome)) +
#   geom_hline(yintercept = 1, size = 1, color = "gray40", linetype = 2) +
#   geom_errorbar(aes(ymin = conf_low, ymax = conf_high), size = 1, width = 0.2) +
#   geom_point(size = 3) +
#   ylim(0, NA) +
#   labs(
#     title = "Vaccination:Any cancer interaction odds ratio for COVID-19 outcome",
#     subtitle = "Yes vs no (reference)",
#     x = "Outcome", y = "Odds ratio (95% confidence interval)"
#   ) +
#   scale_color_brewer(palette = "Set2") +
#   theme_classic() +
#   theme(
#     plot.title = element_text(face = "bold"),
#     legend.position = "none"
#   )
# 
# purrr::map(tmp,
#            ~.x[term == "ComorbidityScore"]) |>
#   rbindlist()|>
#   DT(, outcome := factor(outcome, levels = c("Severe COVID", "Hospitalization", "ICU Admission", "Deceased"))) |>
#   ggplot(aes(x = outcome, y = estimate, color = outcome)) +
#   geom_hline(yintercept = 1, size = 1, color = "gray40", linetype = 2) +
#   geom_errorbar(aes(ymin = conf_low, ymax = conf_high), size = 1, width = 0.2) +
#   geom_point(size = 3) +
#   ylim(0, NA) +
#   labs(
#     title = "Comorbidity score odds ratio for COVID-19 outcome",
#     subtitle = "numeric; 0 to 6",
#     x = "Outcome", y = "Odds ratio (95% confidence interval)"
#   ) +
#   scale_color_brewer(palette = "Set2") +
#   theme_classic() +
#   theme(
#     plot.title = element_text(face = "bold"),
#     legend.position = "none"
#   )
