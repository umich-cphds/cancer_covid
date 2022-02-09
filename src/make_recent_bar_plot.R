make_recent_bar_plot <- function(data_input) {
  
  sum_tab <- data.table(
    comp = c(rep("Severe COVID", 3), rep("Hospitalization", 3), rep("ICU admission", 3), rep("Fatality", 3)),
    status = rep(c("All", "Cancer", "No cancer"), 4),
    outcome = c(
      data_input[in_phenome == 1 & `Severe COVID` == 1, .N],
      data_input[in_phenome == 1 & `Severe COVID` == 1 & recent_AnyCancerPhe == 1, .N],
      data_input[in_phenome == 1 & `Severe COVID` == 1 & recent_AnyCancerPhe == 0, .N],
      
      data_input[in_phenome == 1 & Hospitalized == 1, .N],
      data_input[in_phenome == 1 & Hospitalized == 1 & recent_AnyCancerPhe == 1, .N],
      data_input[in_phenome == 1 & Hospitalized == 1 & recent_AnyCancerPhe == 0, .N],
      
      data_input[in_phenome == 1 & ICU == 1, .N],
      data_input[in_phenome == 1 & ICU == 1 & recent_AnyCancerPhe == 1, .N],
      data_input[in_phenome == 1 & ICU == 1 & recent_AnyCancerPhe == 0, .N],
      
      data_input[in_phenome == 1 & Deceased == 1, .N],
      data_input[in_phenome == 1 & Deceased == 1 & recent_AnyCancerPhe == 1, .N],
      data_input[in_phenome == 1 & Deceased == 1 & recent_AnyCancerPhe == 0, .N]
    ),
    total   = rep(c(
      data_input[in_phenome == 1, .N],
      data_input[in_phenome == 1 & recent_AnyCancerPhe == 1, .N],
      data_input[in_phenome == 1 & recent_AnyCancerPhe == 0, .N]), 4)
  )
  
  sum_tab[, prop := outcome / total][, `:=` (
    SE      = 100 * sqrt(prop * (1 - prop) / total),
    Percent = 100 * prop,
    comp    = factor(comp, levels = c("Severe COVID", "Hospitalization", "ICU admission", "Fatality")))
  ]
  
  display <- paste0(trimws(format(sum_tab[, outcome], big.mark = ",")),
                    "\n out of \n",
                    trimws(format(sum_tab[, total], big.mark = ",")),
                    "\n(",
                    trimws(format(round(sum_tab[, outcome] * 100 / sum_tab[, total], 1), nsmall = 1)),
                    "%)")
  
  bar_plot <- ggplot(data = sum_tab, aes(x = status, y = Percent)) +
    geom_col(aes(fill = status)) +
    geom_errorbar(aes(x = status, ymin = Percent - 1.96 * SE, ymax = Percent + 1.96 * SE), width = 0.5) + 
    facet_grid(. ~ comp) +
    xlab("Cancer status") +
    geom_text(aes(label = display,  y = Percent + 1.96 * SE), vjust = -0.2, position = position_dodge(width = 0.9), size = 3.25) +
    coord_cartesian(ylim = c(0, 23)) +
    scale_fill_brewer(palette = "Set2") +
    ggtitle("COVID-19 outcomes by recent cancer status") +
    labs(fill = "Cancer status",
         caption = "**Note:** Intervals shown represent 95% confidence intervals") +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      # legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, 'cm'),
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      plot.caption = ggtext::element_markdown(hjust = 0),
      legend.position = "bottom",
      axis.text = element_text(size = 10),
      legend.title = element_blank()
    )
  
  pdf(file = "objects/recent_bar_plot.pdf", width = 9,  height = 6)
  print(bar_plot)
  dev.off()
  
  return(print(bar_plot))
  
}