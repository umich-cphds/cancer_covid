make_cancer_by_vax_plot <- function(outcome, title, cancer_var = "AnyCancerPhe", env_name = .GlobalEnv, kable_digits = NULL, ref_level = "0", chrt_vsn = "20220801") {
  
  unvax_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + factor(i2020) + {paste(adj_sets[['adj3']], collapse = ' + ')}, data = main %>% mutate(vax_status = relevel(factor(vax_status), ref = 'Before vaccination'), {cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  pvax_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + factor(i2020) + {paste(adj_sets[['adj3']], collapse = ' + ')}, data = main %>% mutate(vax_status = relevel(factor(vax_status), ref = 'Partially vaccinated'), {cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  fvax_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + factor(i2020) + {paste(adj_sets[['adj3']], collapse = ' + ')}, data = main %>% mutate(vax_status = relevel(factor(vax_status), ref = 'Fully vaccinated (no booster)'), {cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  bvax_mod_text <- glue("logistf({outcome} ~ factor(vax_status) + factor({cancer_var}) + factor(vax_status):factor({cancer_var}) + factor(i2020) + {paste(adj_sets[['adj3']], collapse = ' + ')}, data = main %>% mutate(vax_status = relevel(factor(vax_status), ref = 'Boosted'), {cancer_var} = relevel(factor({cancer_var}), ref = '{ref_level}')), control = logistf.control(maxit = 1000))")
  
  # run models -----------
  unvax_mod <- eval(parse(text = unvax_mod_text), envir = env_name)
  pvax_mod <- eval(parse(text = pvax_mod_text), envir = env_name)
  fvax_mod <- eval(parse(text = fvax_mod_text), envir = env_name)
  bvax_mod <- eval(parse(text = bvax_mod_text), envir = env_name)
  
  unvax <- extract_vax_results(x = unvax_mod, ref = "Before vaccination", trm = "factor(AnyCancerPhe)1")
  pvax <- extract_vax_results(x = pvax_mod, ref = "Partially vaccinated", trm = "factor(AnyCancerPhe)1")
  fvax <- extract_vax_results(x = fvax_mod, ref = "Fully vaccinated (no booster)", trm = "factor(AnyCancerPhe)1")
  bvax <- extract_vax_results(x = bvax_mod, ref = "Boosted", trm = "factor(AnyCancerPhe)1")
  
  vax <- rbindlist(list(unvax, pvax, fvax, bvax))
  
  plot_data <- dcast(vax[stat %in% c("or", "lower", "upper")], vax_reference ~ stat)
  
  plot_data[vax_reference == "Before vaccination", order := 1]
  plot_data[vax_reference == "Partially vaccinated", order := 2]
  plot_data[vax_reference == "Fully vaccinated (no booster)", order := 3]
  plot_data[vax_reference == "Boosted", order := 4]
  
  n_data <- data.table(
    vax_reference = c("Before vaccination", "Partially vaccinated", "Fully vaccinated (no booster)", "Boosted"),
    order = c(1:4),
    n = c(main[vax_status == "Before vaccination", .N], main[vax_status == "Partially vaccinated", .N], main[vax_status == "Fully vaccinated (no booster)", .N], main[vax_status == "Boosted", .N])
  )[, text := paste0("n = ", format(n, big.mark = ","))][]
  
  cols <- c(
    "Before vaccination" = RColorBrewer::brewer.pal(n = 4, name = "Set1")[1],
    "Partially vaccinated" = RColorBrewer::brewer.pal(n = 4, name = "Set1")[2],
    "Fully vaccinated (no booster)" = RColorBrewer::brewer.pal(n = 4, name = "Set1")[3],
    "Boosted" = RColorBrewer::brewer.pal(n = 4, name = "Set1")[4]
  )
  
  tmp_plot <- plot_data %>%
    ggplot(aes(x = reorder(vax_reference, order), y = or, color = vax_reference)) +
    geom_hline(yintercept = 1, linetype = 2, color = "gray40") +
    geom_errorbar(aes(ymin = lower, ymax = upper), size = 1, width = 0.2) +
    geom_point(size = 2) +
    ylim(0, max(plot_data[, max(upper)] + 0.4, (1.02142857*plot_data[, max(upper)]))) +
    scale_color_manual(values = cols) +
    
    annotate(geom = "label", x = n_data[, vax_reference], y = 0,
             label = n_data[, text], hjust = 0.5, vjust = 0.5, size = 3, color = cols) +
    
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
      title = glue("ORs for COVID-19 outcomes corresponding to cancer status\nstratified by vaccination status"),
      x = "Vaccination status",
      y = "Cancer status odds ratio (95% CI)"
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
  
  ggsave(filename = paste0("objects/", chrt_vsn, "/", outcome, "_cancer_by_vax_plot.pdf"), width = 8, height = 4,
         plot = tmp_plot)
  
  cli::cli_alert_success("plot saved at objects/{chrt_vsn}/{outcome}_cancer_by_vax_plot.pdf")
  
}
