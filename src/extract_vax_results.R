extract_vax_results <- function(x, ref, trm) {
  tmp <- broomExtra::tidy_parameters(x, ci_method = "wald") |> as.data.table()
  
  out <- data.table(
    "stat" = c("beta", "se_beta", "conf_lo", "conf_hi", "p_val"),
    "value"  = c(tmp[term == trm, estimate], tmp[term == trm, std.error],
                 tmp[term == trm, conf.low], tmp[term == trm, conf.high], tmp[term == trm, p.value])
  )
  
  out <- rbindlist(list(
    out,
    data.table(
      "stat" = c("or", "lower", "upper"),
      "value"  = c(
        exp(out[stat == "beta", value]),
        exp(out[stat == "conf_lo", value]),
        exp(out[stat == "conf_hi", value]))
    )
  ))
  
  
  return(out[, vax_reference := ref][])
  
}
