## libraries
library(data.table)
library(ggplot2)
library(ggtext)

## load vax ever data by year
tmp_2020 <- fread("../cancer_covid_revision/objects/vax_ev_2020.csv")[term == "vax_ever"][, year := "2020"]
tmp_2021 <- fread("../cancer_covid_revision/objects/vax_ev_2021.csv")[term == "vax_ever"][, year := "2021"]
tmp_2022 <- fread("../cancer_covid_revision/objects/vax_ev_2022.csv")[term == "vax_ever"][, year := "2022"]

## plot
rbindlist(list(
  tmp_2020, tmp_2021, tmp_2022
)) |>
  ggplot(aes(x = year, y = estimate)) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray40", size = 1) +
  geom_pointrange(aes(ymin = conf_low, ymax = conf_high), size = 1) +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3), limits = c(0, 3)) +
  # coord_cartesian(ylim = c(0, 2)) +
  labs(
    title = "Vaccine (ever/never) OR for severe COVID by year",
    x = "Year",
    y = "OR (95% CI)",
    caption = "**Notes:**<br>
      - Error bars indicated 95% confidence interval<br>
      - Adjusted for age, sex, race/ethnicity, disadvantage index (quartile), cancer status (ever/never), and year"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = ggtext::element_markdown(hjust = 0)
  )

## save
ggsave(filename = glue::glue("../cancer_covid_revision/objects/vax_by_year_plot.pdf"), width = 8, height = 5.5)
