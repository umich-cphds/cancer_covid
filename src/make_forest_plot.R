make_forest_plot <- function(no_cancer_results, cancer_results, outcome, other = "", y_intercept = 28.5, chrt_vsn = "20220801") {
  
  tab <- merge.data.table(
    as.data.table(cancer_results[[outcome]])[, .(term, cancer_print, cancer_p_value)],
    as.data.table(no_cancer_results[[outcome]])[, .(term, no_cancer_print, no_cancer_p_value, p_int_value, color)],
    by = "term",
    sort = FALSE
  )
  setcolorder(tab, c("term", "cancer_print", "no_cancer_print", "cancer_p_value", "no_cancer_p_value", "p_int_value", "color"))
  tab[, face := "plain"]
  
  tab_labs_1 = c(' ', 'Cancer','No Cancer', 'P-Value', 'P-Value', 'P-Value', 'white', 'bold')
  tab_labs_2 = c('Variable', 'OR (95% CI)','OR (95% CI)', 'Cancer', 'No Cancer', 'Interaction', 'white', 'bold')
  
  tab <- rbind(
    as.list(tab_labs_1),
    as.list(tab_labs_2),
    tab
  )
  
  tab[, term := factor(term, levels = rev(unique(term)))]
  
  setnames(tab,
           c("cancer_p_value", "no_cancer_p_value", "p_int_value", "cancer_print", "no_cancer_print"),
           c("p-value for cancer", "p-value for no cancer", "p-value for interaction", "Cancer OR for susceptibility (95% CI)", "No cancer OR for susceptibility (95% CI)"))
  
  
  long <- rbindlist(list(
    as.data.table(cancer_results[[outcome]])[, .(term, or = cancer_est, lower = cancer_low, higher = cancer_high, color)][, type := "Cancer"],
    as.data.table(no_cancer_results[[outcome]])[, .(term, or = no_cancer_est, lower = no_cancer_low, higher = no_cancer_high, color)][, type := "No cancer"]
  ))
  
  long[, face := "plain"]
  
  long_labs_1 = c('L1', NA, NA,NA,'white', 'Cancer', 'bold')
  long_labs_2 = c('L2', NA, NA,NA,'white', 'Cancer', 'bold')
  
  long <- rbind(
    as.list(long_labs_1),
    as.list(long_labs_2),
    long
  )
  long[, term := factor(term, levels = rev(unique(term)))]
  
  tmp_forest <- ggplot(long, aes(x = as.numeric(or), y = term,
                                 xmin = as.numeric(lower),
                                 xmax = as.numeric(higher))) +
    geom_hline(aes(yintercept = term, color = color), size = 7) + 
    geom_hline(aes(yintercept = y_intercept, colour = 'black'), size = 0.5) + 
    geom_vline(xintercept = 1, linetype = 3) +
    geom_pointrange(aes(fill = type, shape = type), position = ggstance::position_dodgev(height=1)) +
    xlab("OR (95% CI)") +
    theme_classic() +
    scale_colour_identity() +
    scale_shape_manual(values = c(22,23),name="") +
    scale_x_log10(breaks = c(0.25, 0.5, 1, 1.5, 2, 3),
                  label = c(0.25, 0.5, 1, 1.5, 2, 3)) +
    coord_cartesian(xlim = c(0.2, 4)) +
    scale_fill_discrete(name="") + 
    scale_y_discrete(breaks = long$term)+
    guides(fill=guide_legend(""))+
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())+
    theme(legend.position = c(0.5, 0.96), 
          legend.background = element_rect(fill="white",size=0.5, linetype="solid", colour ="white"), 
          legend.direction = 'horizontal',
          legend.text = element_text(size = 12))
  
  ors_table <- ggplot(data = tab, aes(y = factor(term))) +
    geom_hline(aes(yintercept = term, colour = color), size = 7) +
    geom_text(aes(x = 0, label = term, fontface = face), hjust = 0) +
    geom_text(aes(x = 5, label = `Cancer OR for susceptibility (95% CI)`, fontface = face), hjust = 1) +
    geom_text(aes(x = 7, label = `No cancer OR for susceptibility (95% CI)`, fontface = face), hjust = 1) +
    scale_colour_identity() +
    theme_void() + 
    theme(plot.margin = margin(5, 0, 35, 0))+
    geom_hline(aes(yintercept = y_intercept, colour = 'black'), size = 0.5) 
  
  pvalues_table <- ggplot(data = tab, aes(y = term)) +
    geom_hline(aes(yintercept = term, colour = color), size = 7) +
    geom_text(aes(x = 0, label = `p-value for cancer`, fontface = face), hjust = 0.5) +
    geom_text(aes(x = 0.20, label = `p-value for no cancer`, fontface = face), hjust = 0.5) +
    geom_text(aes(x = 0.45, label = `p-value for interaction`, fontface = face), hjust = 0.5) +
    scale_colour_identity() +
    xlim(-0.1,0.55)+
    theme_void() + 
    theme(plot.margin = margin(5, 0, 35, 0))+
    geom_hline(aes(yintercept = y_intercept, colour = 'black'), size = 0.5)
  
  pdf(file = paste0("objects/", chrt_vsn, "/", outcome, other, "_forest_plot.pdf"), width = 12,  height = 10)
  print(cowplot::plot_grid(ors_table, tmp_forest, pvalues_table, align = "h",
                           ncol = 3, rel_widths = c(4/8, 2.4/8,1.6/8)))
  dev.off()
  
  return(
    print(cowplot::plot_grid(ors_table, tmp_forest, pvalues_table, align = "h",
                             ncol = 3, rel_widths = c(4/8, 2.4/8,1.6/8)))
  )
  
}
