# save chemical code lookup tables for each year
library(readr)
library(usethis)

chemical_list <- list()

for (year in as.character(1990:2015)) {

  url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
                year, ".zip")
  file <- paste0("pur", year, ".zip")

  current_dir <- getwd()
  dir <- tempdir()

  download.file(url, destfile = file, mode = "wb", quiet = quiet)
  unzip(file, exdir = dir)
  setwd(dir)

  chemical_file <- readr::read_csv("chemical.txt")
  chemical_list[[year]] <- chemical_file

  setwd(current_dir)

}

usethis::use_data(chemical_list, internal = TRUE, overwrite = TRUE)
