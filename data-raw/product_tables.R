# save product tables for each year
library(readr)
library(usethis)
library(purrr)
library(dplyr)

pull_product_table <- function(year) {

  url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
                year, ".zip")
  file <- paste0("pur", year, ".zip")
  current_dir <- getwd()
  dir <- tempdir()

  download.file(url, destfile = file, mode = "wb")
  unzip(file, exdir = dir)
  setwd(dir)

  product_file <- readr::read_csv("product.txt") %>%
    dplyr::select(prodno, prodstat_ind, prodcut_name, show_regno,
                  aer_grnd_ind, formula_cd, full_exp_dt, full_iss_dt,
                  fumigant_sw, gen_pest_ind, lastup_dt, mfg_ref_sw,
                  prod_inac_dt, reg_dt, reg_type_ind, rodent_sw, rignlwrd_ind,
                  soilappl_sw, condreg_sw)

  setwd(current_dir)

  return(product_file)

}

pull_product_table <- function(year) {

  dir <- paste0("~/Documents/pesticides_project/data-raw/PUR/", year)

  current_dir <- getwd()
  setwd(dir)

  product_file <- readr::read_csv("product.txt") %>%
    dplyr::select(prodno, prodstat_ind, product_name, signlwrd_ind)

  setwd(current_dir)

  return(product_file)

}

product_list <- purrr::map(1990:2015, pull_product_table)
names(product_list) <- 1990:2015

usethis::use_data(product_list, overwrite = TRUE)

