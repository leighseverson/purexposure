# save county codes file from one year
library(readr)
library(usethis)

year <- "2000"
quiet <- TRUE
url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
              year, ".zip")
file <- paste0("pur", year, ".zip")

current_dir <- getwd()
dir <- tempdir()

download.file(url, destfile = file, mode = "wb", quiet = quiet)
unzip(file, exdir = dir)
setwd(dir)

county_codes <- readr::read_csv("county.txt")
setwd(current_dir)
usethis::use_data(county_codes, overwrite = TRUE)
