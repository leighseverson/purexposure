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
  dplyr::select(county_name, county_code)

setwd(current_dir)

usethis::use_data(county_codes, overwrite = TRUE)
