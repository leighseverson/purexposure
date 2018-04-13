library(magrittr)

# save toy datasets to use for examples
fresno_ex <- pull_clean_pur(2000, "fresno")
fresno_ex <- fresno_ex %>%
  dplyr::filter(date >= lubridate::ymd("2000-01-01") &
                  date <= lubridate::ymd("2000-01-31"))

usethis::use_data(fresno_ex, overwrite = TRUE)

exposure_ex <- purexposure::fresno_ex %>%
  calculate_exposure(location = "-119.726751, 36.660967", radius = 3000)

exposure_ex$county_plot <- NULL

usethis::use_data(exposure_ex, overwrite = TRUE)

exposure_ex2 <- purexposure::fresno_ex %>%
  calculate_exposure(location = "-119.247100, 37.204875", radius = 3000)

exposure_ex2$county_plot <- NULL

usethis::use_data(exposure_ex2, overwrite = TRUE)
