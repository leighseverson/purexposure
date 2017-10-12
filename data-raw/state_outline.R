library(ggplot2)
library(rgdal)
library(sp)
library(broom)

# downloaded from here: https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
# 1:20,000,000 resolution level

current_dir <- getwd()
setwd("~/Downloads/cb_2016_us_state_20m")
shp_file <- "cb_2016_us_state_20m.shp"

shp <- rgdal::readOGR(shp_file,
                      layer = basename(strsplit(shp_file, "\\.")[[1]])[1],
                      verbose = FALSE)
setwd(current_dir)

shp <- sp::spTransform(shp, sp::CRS("+init=epsg:4326"))

california_shp <- shp[shp$STUSPS == "CA",]

usethis::use_data(california_shp, overwrite = TRUE)
