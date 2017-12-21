# save county codes file from one year
library(readr)
library(usethis)
library(dplyr)

year <- "2000"
quiet <- TRUE
url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
              year, ".zip")
file <- paste0("pur", year, ".zip")

current_dir <- getwd()
dir <- tempdir()
setwd(dir)

download.file(url, destfile = file, mode = "wb", quiet = quiet)
unzip(file, exdir = dir)

county_codes <- readr::read_csv("county.txt")
county_codes <- county_codes %>%
  dplyr::rename(county_name = couty_name,
                county_code = county_cd) %>%
  dplyr::select(county_name, county_code) %>%
  dplyr::filter(county_name != "UNKNOWN") # no "unknown" files in 1990:2015

setwd(current_dir)

fips_codes <- readr::read_csv(paste0("http://www2.census.gov/geo/docs/reference",
                                     "/cenpop2010/county/CenPop2010_Mean_CO.txt"))
fips_codes <- fips_codes %>% dplyr::filter(STNAME == "California") %>%
  tidyr::unite(fips_code, 1:2, sep = "") %>%
  dplyr::mutate(county_name = toupper(COUNAME)) %>%
  dplyr::select(county_name, fips_code)

county_codes <- county_codes %>% plyr::rename(c("county_code" = "pur_code")) %>%
  dplyr::full_join(fips_codes, by = "county_name")

usethis::use_data(county_codes, overwrite = TRUE)
