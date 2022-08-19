make_cancer_by_vax_plot_2.0 <- function(clean_res) {
  
cols <- c(
  "Before vaccination" = RColorBrewer::brewer.pal(n = 4, name = "Set1")[1],
  "Partially vaccinated" = RColorBrewer::brewer.pal(n = 4, name = "Set1")[2],
  "Fully vaccinated (no booster)" = RColorBrewer::brewer.pal(n = 4, name = "Set1")[3],
  "Boosted" = RColorBrewer::brewer.pal(n = 4, name = "Set1")[4]
)

clean_res |>
  mutate(
    Outcome = factor(case_when(
      outcome == "Severe COVID" ~ "Severe COVID",
      outcome == "Hospitalized" ~ "Hospitalization",
      outcome == "ICU" ~ "ICU admission",
      outcome == "Deceased" ~ "Mortality"
    ), levels = c("Severe COVID", "Hospitalization", "ICU admission", "Mortality")),
    Term = vax_status,
    Term = ifelse(Term == "Fully vaccinated (no booster)", "Fully vaccinated\n(no booster)", Term),
    Term = factor(Term, levels = c("Before vaccination", "Partially vaccinated", "Fully vaccinated\n(no booster)", "Boosted"))
  ) |>
  ggplot(aes(x = Term, y = estimate, color = vax_status)) +
  scale_color_manual(values = cols) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray40") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                width = 0.2, size = 1) +
  facet_wrap(~Outcome, nrow = 1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  coord_cartesian(ylim = c(0, 4)) +
  labs(
    title = glue("ORs for COVID-19 outcomes corresponding to cancer status\nstratified by vaccination status"),
    x = "Vaccination status",
    y = "Cancer status odds ratio (95% CI)"
  ) +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold"),
    legend.title     = element_blank(),
    legend.position  = "none",
    plot.caption     = ggtext::element_markdown(hjust = 0),
    panel.spacing.x  = unit(1, "lines"))
}

