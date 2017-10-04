library(readr)
library(dplyr)
library(devtools)
library(usethis)

# save raw PUR data for 1990 through 2015
# list$county$year

county_codes <- readr::read_csv("inst/extdata/2015/county.txt")$county_cd
years <- list.files("inst/extdata")
raw_data_list <- list()

for (i in 1:length(county_codes)) {

  county_list <- list()

  for (j in 1:length(years)) {

    sm_year <- substr(years[j], 3, 4)
    county_file <- grep(paste0("udc", sm_year, "_", county_codes[i]),
                        list.files(paste0("inst/extdata/", years[j])),
                        value = TRUE)
    county_df <- readr::read_csv(paste0("inst/extdata/", years[j], "/",
                                        county_file))


    county_list[[years[j]]] <- county_df

  }

  raw_data_list[[county_codes[i]]] <- county_list

}

usethis::use_data(raw_data_list, internal = TRUE)
# way too large. 1.5 GB- should be less than a megabyte.
