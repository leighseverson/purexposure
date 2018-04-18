library(magrittr)
# save toy datasets to use for examples

# fresno pur raw
fresno_raw <- pull_raw_pur(2000, "fresno")
fresno_raw <- fresno_raw %>%
  dplyr::mutate(applic_dt = lubridate::ymd(applic_dt)) %>%
  dplyr::filter(applic_dt >= lubridate::ymd("2000-01-01") &
                  applic_dt <= lubridate::ymd("2000-01-31")) %>%
  dplyr::mutate(applic_dt = as.character(applic_dt))
saveRDS(fresno_raw, file = "inst/extdata/fresno_raw.rds")

# fesno pur clean
fresno_clean <- pull_clean_pur(2000, "fresno")
fresno_clean <- fresno_clean %>%
  dplyr::filter(date >= lubridate::ymd("2000-01-01") &
                  date <= lubridate::ymd("2000-01-31"))
saveRDS(fresno_clean, file = "inst/extdata/fresno_clean.rds")

# fresno exposure #1
fresno_clean <- readRDS(system.file("extdata", "fresno_clean.rds",
                                    package = "purexposure"))
exposure_ex <- fresno_clean %>%
  calculate_exposure(location = "-119.726751, 36.660967", radius = 3000)
exposure_ex$county_plot <- NULL
saveRDS(exposure_ex, file = "inst/extdata/exposure_ex.rds")

# fresno exposure #2
exposure_ex2 <- fresno_clean %>%
  calculate_exposure(location = "-119.247100, 37.204875", radius = 3000)
exposure_ex2$county_plot <- NULL
saveRDS(exposure_ex2, file = "inst/extdata/exposure_ex2.rds")

# fresno spdf
fresno_spdf <- pull_spdf("fresno")
saveRDS(fresno_spdf, file = "inst/extdata/fresno_spdf.rds")

# products table for 2000
products_2000 <- pull_product_table(2000)
saveRDS(products_2000, file = "inst/extdata/products_2000.rds")
