library(magrittr)
# save toy datasets to use for examples

# fresno pur raw
fresno_raw <- pull_raw_pur(2000, "fresno")
fresno_raw <- fresno_raw %>%
  dplyr::mutate(applic_dt = lubridate::ymd(applic_dt)) %>%
  dplyr::filter(applic_dt >= lubridate::ymd("2000-01-01") &
                  applic_dt <= lubridate::ymd("2000-01-31")) %>%
  dplyr::mutate(applic_dt = as.character(applic_dt))
usethis::use_data(fresno_raw, overwrite = TRUE)

# fesno pur clean
fresno_clean <- pull_clean_pur(2000, "fresno")
fresno_clean <- fresno_clean %>%
  dplyr::filter(date >= lubridate::ymd("2000-01-01") &
                  date <= lubridate::ymd("2000-01-31"))
usethis::use_data(fresno_clean, overwrite = TRUE)

# fresno exposure #1
exposure_ex <- purexposure::fresno_clean %>%
  calculate_exposure(location = "-119.726751, 36.660967", radius = 3000)
exposure_ex$county_plot <- NULL
usethis::use_data(exposure_ex, overwrite = TRUE)

# fresno exposure #2
exposure_ex2 <- purexposure::fresno_clean %>%
  calculate_exposure(location = "-119.247100, 37.204875", radius = 3000)
exposure_ex2$county_plot <- NULL
usethis::use_data(exposure_ex2, overwrite = TRUE)

# fresno spdf
fresno_spdf <- pull_spdf("fresno")
usethis::use_data(fresno_spdf, overwrite = TRUE)

# products table for 2000
products_2000 <- pull_product_table(2000)
usethis::use_data(products_2000, overwrite = TRUE)
