# get vaccine status prior to first positive test variable -----------
get_pre_positive_vax_status <- function(x, days_before = 14) {
  
  # subset to individuals with non-missing vaccine data
  vax <- x[!is.na(Vaccine)]
  
  # drop individuals with missing DSB for vaccine data
  missing_first_vax <- vax[!is.na(Vaccine) & (is.na(DaysSinceBirth_FirstVaccination)), unique(id)]
  missing_full_vax  <- vax[!is.na(Vaccine) & (TotalNumberVaccinations > 1 & is.na(DaysSinceBirth_FullyVaccinated)), unique(id)]
  vax               <- vax[!(id %in% c(missing_first_vax, missing_full_vax))]
  
  # limit to FDA-approved vaccines
  fda_approved <- c("Janssen", "Moderna", "Pfizer")
  vax          <- vax[Vaccine %in% fda_approved]
  
  # create vaccine status prior to first positive tested indicator variables
  # UNVACCINATED
  vax <- vax[, `:=` (
    unvaccinated = as.numeric(DaysSinceBirth_First_PositiveTestDiagnosis <= (DaysSinceBirth_FirstVaccination - days_before))
  )]
  
  # PARTIALLY VACCINATED
  vax <- vax[, `:=` (
    partial_vax = as.numeric((DaysSinceBirth_First_PositiveTestDiagnosis > (DaysSinceBirth_FirstVaccination - days_before)) &
                               (DaysSinceBirth_First_PositiveTestDiagnosis < (DaysSinceBirth_FullyVaccinated - days_before)))
  )]
  
  # FULLY VACCINATED NO BOOSTER
  vax <- vax[, `:=` (
    full_vax = as.numeric((DaysSinceBirth_First_PositiveTestDiagnosis >= (DaysSinceBirth_FullyVaccinated - days_before)) &
                            (DaysSinceBirth_First_PositiveTestDiagnosis < (DaysSinceBirth_FirstBooster + 21 - days_before)))
  )]
  
  # FULLY VACCINATED WITH BOOSTER
  vax <- vax[, `:=` (
    boosted = as.numeric((DaysSinceBirth_First_PositiveTestDiagnosis >= (DaysSinceBirth_FullyVaccinated - days_before)) &
                           (DaysSinceBirth_First_PositiveTestDiagnosis >= (DaysSinceBirth_FirstBooster + 21 - days_before)))
  )]
  
  # create a single vaccine status variable
  vax[, vax_status := "Before vaccination"]
  vax[boosted == 1, vax_status := "Boosted"]
  vax[full_vax == 1, vax_status := "Fully vaccinated (no booster)"]
  vax[partial_vax == 1, vax_status := "Partially vaccinated"]
  vax[id %in% c(missing_first_vax, missing_full_vax), vax_status := NA]
  
  out <- merge.data.table(
    x[in_phenome == 1],
    vax[, .(id, vax_status)],
    by = "id",
    all.x = TRUE
  )
  
  out[is.na(Vaccine), vax_status := "Before vaccination"]
  
  # replace individuals with non-fda approved vaccines as missing
  out[(!(Vaccine %in% fda_approved) & !is.na(Vaccine)), vax_status := NA][]
  
}
