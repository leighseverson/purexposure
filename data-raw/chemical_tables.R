# save chemical code lookup tables for each year
library(readr)
library(usethis)
library(purrr)
library(dplyr)

pull_chemical_list <- function(year) {

  url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
                year, ".zip")
  file <- paste0("pur", year, ".zip")

  current_dir <- getwd()
  dir <- tempdir()

  download.file(url, destfile = file, mode = "wb")
  unzip(file, exdir = dir)
  setwd(dir)

  if (year > 2015) {
    setwd(paste0("pur", year))
    chemical_file <- readr::read_csv("chemical.txt") %>%
      dplyr::select(-chemalpha_cd)
    setwd("..")
  } else {
    chemical_file <- readr::read_csv("chemical.txt") %>%
      dplyr::select(-chemalpha_cd)
  }

  setwd(current_dir)

  return(chemical_file)

}

chemical_list <- purrr::map(1990:2016, pull_chemical_list)
names(chemical_list) <- 1990:2016

usethis::use_data(chemical_list, overwrite = TRUE)
