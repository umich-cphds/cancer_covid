make_cancer_by_vax_plot <- function(outcome, title, cancer_var = "AnyCancerPhe", env_name = .GlobalEnv, kable_digits = NULL, ref_level = "0") {
  
  unvax_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + factor(i2020) + {paste(adj_sets[['adj3']], collapse = ' + ')}, data = main %>% mutate(vax_status = relevel(factor(vax_status), ref = 'Prior to vaccination'), {cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  pvax_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + factor(i2020) + {paste(adj_sets[['adj3']], collapse = ' + ')}, data = main %>% mutate(vax_status = relevel(factor(vax_status), ref = 'After one dose'), {cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  fvax_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + factor(i2020) + {paste(adj_sets[['adj3']], collapse = ' + ')}, data = main %>% mutate(vax_status = relevel(factor(vax_status), ref = 'After two doses'), {cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  bvax_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + factor(i2020) + {paste(adj_sets[['adj3']], collapse = ' + ')}, data = main %>% mutate(vax_status = relevel(factor(vax_status), ref = 'After booster'), {cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  
  # run models -----------
  unvax_mod <- eval(parse(text = unvax_mod_text), envir = env_name)
  pvax_mod <- eval(parse(text = pvax_mod_text), envir = env_name)
  fvax_mod <- eval(parse(text = fvax_mod_text), envir = env_name)
  bvax_mod <- eval(parse(text = bvax_mod_text), envir = env_name)
  
  unvax <- extract_vax_results(x = unvax_mod, ref = "Prior to vaccination", trm = "factor(AnyCancerPhe)1")
  pvax <- extract_vax_results(x = pvax_mod, ref = "After one dose", trm = "factor(AnyCancerPhe)1")
  fvax <- extract_vax_results(x = fvax_mod, ref = "After two doses", trm = "factor(AnyCancerPhe)1")
  bvax <- extract_vax_results(x = bvax_mod, ref = "After booster", trm = "factor(AnyCancerPhe)1")
  
  vax <- rbindlist(list(unvax, pvax, fvax, bvax))
  
  plot_data <- dcast(vax[stat %in% c("or", "lower", "upper")], vax_reference ~ stat)
  
  plot_data[vax_reference == "Prior to vaccination", order := 1]
  plot_data[vax_reference == "After one dose", order := 2]
  plot_data[vax_reference == "After two doses", order := 3]
  plot_data[vax_reference == "After booster", order := 4]
  
  tmp_plot <- plot_data %>%
    ggplot(aes(x = reorder(vax_reference, order), y = or)) +
    geom_hline(yintercept = 1, linetype = 2, color = "gray40") +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    ylim(0, max(plot_data[, max(upper)] + 0.4, (1.02142857*plot_data[, max(upper)]))) +
    
    annotate(geom = "text", x = plot_data[, vax_reference], y = plot_data[, upper] + (0.02142857*plot_data[, max(upper)]),
             label = paste0(sprintf("%.2f", round(plot_data[, or], 2)),
                            "\n(",
                            sprintf("%.2f", round(plot_data[, lower], 2)),
                            ", ",
                            sprintf("%.2f", round(plot_data[, upper], 2)),
                            ")"),
             hjust = 0.5, vjust = 0) +
    
    # geom_point() +
    labs(
      title = glue("Cancer diagnosis OR for {title} by level of vaccination status"),
      x = "Vaccination status",
      y = "Odds ratio (95% CI)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold")
    )
  
  ggsave(filename = glue("objects/{outcome}_cancer_by_vax_plot.pdf"), width = 7, height = 5,
         plot = tmp_plot)
  
  cli::cli_alert_success("plot saved at objects/{outcome}_cancer_by_vax_plot.pdf")
  
}
