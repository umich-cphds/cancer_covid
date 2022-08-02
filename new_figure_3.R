library(tidyverse)
library(data.table)
library(ggtext)
library(RColorBrewer)
library(glue)

d  <- read_rds("objects/vax_analysis.rds")
dc <- read_rds("objects/vax_analysis_cancer_reference.rds")

d$severe_covid$full %>%
  filter(term %in% paste0("factor(vax_status)", c(
    "Boosted",
    "Fully vaccinated (no booster)",
    "Partially vaccinated"))
    ) %>%
  mutate(
    cancer_status = "No cancer"
  )

pull_data <- function(no_cancer_results, cancer_results, outcome) {
  
  ncr <- no_cancer_results[[outcome]]$full %>%
    filter(term %in% paste0("factor(vax_status)", c(
      "Boosted",
      "Fully vaccinated (no booster)",
      "Partially vaccinated"))
    ) %>%
    mutate(
      cancer_status = "No cancer",
      outcome       = outcome
    )
  
  cr  <- cancer_results[[outcome]]$full %>%
    filter(term %in% paste0("factor(vax_status)", c(
      "Boosted",
      "Fully vaccinated (no booster)",
      "Partially vaccinated"))
    ) %>%
    mutate(
      cancer_status = "Cancer",
      outcome       = outcome
    )
  
  bind_rows(
    ncr,
    cr
  )
  
}

plot_data <- function(ncr, cr) {
  
  sev   <- pull_data(no_cancer_results = ncr, cancer_results = cr, outcome = "severe_covid")
  hosp  <- pull_data(no_cancer_results = ncr, cancer_results = cr, outcome = "hospitalized")
  icu   <- pull_data(no_cancer_results = ncr, cancer_results = cr, outcome = "icu")
  death <- pull_data(no_cancer_results = ncr, cancer_results = cr, outcome = "deceased")
  
  bind_rows(
    sev,
    hosp,
    icu,
    death
  )
  
}

interaction_data <- function(res) {
  
  terms <- c(
    "factor(vax_status)Boosted:factor(AnyCancerPhe)1",
    "factor(vax_status)Fully vaccinated (no booster):factor(AnyCancerPhe)1",
    "factor(vax_status)Partially vaccinated:factor(AnyCancerPhe)1"
  )
  
  bind_rows(
    res$severe_covid$full %>% filter(term %in% terms) %>%
      mutate(Outcome = "Severe COVID"),
    res$hosp$full %>% filter(term %in% terms) %>%
      mutate(Outcome = "Hospitalization"),
    res$icu$full %>% filter(term %in% terms) %>%
      mutate(Outcome = "ICU admission"),
    res$deceased$full %>% filter(term %in% terms) %>%
      mutate(Outcome = "Mortality")
  ) %>%
    mutate(
      Term = factor(case_when(
        term == "factor(vax_status)Partially vaccinated:factor(AnyCancerPhe)1" ~ "Partially vaccinated",
        term == "factor(vax_status)Fully vaccinated (no booster):factor(AnyCancerPhe)1" ~ "Fully vaccinated\n(no booster)",
        term == "factor(vax_status)Boosted:factor(AnyCancerPhe)1" ~ "Boosted"
      ), levels = c("Partially vaccinated", "Fully vaccinated\n(no booster)", "Boosted")),
      Outcome = factor(Outcome, levels = c("Severe COVID", "Hospitalization", "ICU admission", "Mortality"))
  ) %>%
    mutate(
      sig = ifelse(p_value < 0.05, 1, NA)
    )
  
}

plt_dat <- plot_data(ncr = d, cr = dc)
int_dat <- interaction_data(res = d)

canc_stat_cols <- c(
  "Cancer"    = "#FC8D62",
  "No cancer" = "#8DA0CB"
)

plt_dat %>%
  mutate(
    Outcome = factor(case_when(
      outcome == "severe_covid" ~ "Severe COVID",
      outcome == "hospitalized" ~ "Hospitalization",
      outcome == "icu" ~ "ICU admission",
      outcome == "deceased" ~ "Mortality"
    ), levels = c("Severe COVID", "Hospitalization", "ICU admission", "Mortality")),
    Term = factor(case_when(
      term == "factor(vax_status)Partially vaccinated" ~ "Partially vaccinated",
      term == "factor(vax_status)Fully vaccinated (no booster)" ~ "Fully vaccinated\n(no booster)",
      term == "factor(vax_status)Boosted" ~ "Boosted"
    ), levels = c("Partially vaccinated", "Fully vaccinated\n(no booster)", "Boosted"))
  ) %>%
  ggplot(aes(x = Term, y = estimate, color = cancer_status)) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray40") +
  geom_point(position = position_dodge(width = .5), size = 2) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                  position = position_dodge(width = .5), width = 0.2, size = 1) +
  geom_point(data = int_dat %>% filter(sig == 1), aes(x = Term, y = 0), shape = 8, color = "black") +
  labs(
    title = "Vaccination status OR for COVID-19 outcomes by cancer status",
    x = "Vaccination status",
    y = "Odds ratio (95% CI)",
    caption = "**Notes:**<br>
      - Error bars indicated 95% confidence interval<br>
      - '*' indicates p-value for interaction significant at 0.05 level"
  ) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_color_manual(values = canc_stat_cols) +
  facet_wrap(~Outcome, nrow = 1) +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold"),
    legend.title     = element_blank(),
    legend.position  = "bottom",
    plot.caption     = ggtext::element_markdown(hjust = 0),
    panel.spacing.x  = unit(1, "lines"))

ggsave(filename = glue("objects/vax_int_plot.pdf"), width = 8, height = 5.5)
