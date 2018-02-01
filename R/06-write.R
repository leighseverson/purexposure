schools <- readRDS("~/Documents/pesticides_project/data/fresno_schools.rds")
schools <- schools[1:3,"full_address"] %>% purexposure:::tibble_to_vector()

schools <- gsub("-.*", "", schools)

schools_df <- find_location_county(schools)
sum(!duplicated(schools_df$county)) == 1

pur <- pull_clean_pur(2000:2001, counties = "fresno", chemicals = "sulfur")
df <- data.frame(location = rep(schools, 2), start_date = rep(c("2000-01-01",
                                                                  "2000-05-25",
                                                                  "2001-02-16"), 2),
                     end_date = c("2000-04-01", "2000-07-01",
                                   "2000-08-25", "2000-11-25",
                                   "2001-05-16", "2001-08-16")) %>%
  dplyr::arrange(location, start_date)

clean_pur_df <- pur
locations_dates_df <- df
radii <- c(1500, 2500, 3000)
chemicals <- "all"
aerial_ground <- FALSE
directory <- "~/Desktop/schools_test"
write_plots <- TRUE

write_exposure <- function(clean_pur_df, locations_dates_df, radii,
                           chemicals = "all", aerial_ground = FALSE,
                           directory, write_plots = TRUE, verbose = TRUE,
                           ...) {

  locations <- as.character(unique(locations_dates_df$location))

  for (i in 1:length(locations)) {
    if (length(grep("-", locations[i])) == 1) {
      latlon <- locations[i]
      latlon_vec <- as.numeric(as.vector(sapply(unlist(strsplit(latlon, ",")),
                                                stringr::str_trim)))
      address_x <- latlon_vec[1]
      address_y <- latlon_vec[2]
      latlon_out <- latlon_vec
    } else {
      address <- locations[i]
      suppressMessages(latlon_df <- ggmap::geocode(address, messaging = FALSE))
      address_x <- latlon_df$lon
      address_y <- latlon_df$lat
      latlon_out <- as.numeric(c(latlon_df$lon, latlon_df$lat))
    }
    latlon_ch <- paste0(latlon_out[1], ", ", latlon_out[2])
    if (i == 1) {
      latlon <- latlon_ch
    } else {
      latlon <- c(latlon, latlon_ch)
    }
  }

  loc_df <- data.frame(location = unique(locations_dates_df$location),
                       latlon_loc = latlon)

  locations_dates_df <- full_join(locations_dates_df, loc_df, by = "location")

  radius_list <- list()
  for (i in 1:length(radii)) {
    radius_list[[i]] <- locations_dates_df %>% mutate(radius = radii[i])
  }
  exposure_mat <- do.call("rbind", radius_list)
  exposure_mat <- exposure_mat %>% dplyr::mutate(chemicals = chemicals)

  if (aerial_ground) {
    exposure_mat <- exposure_mat %>% dplyr::mutate(aerial_ground = TRUE)
  } else {
    exposure_mat <- exposure_mat %>% dplyr::mutate(aerial_ground = FALSE)
  }

  if (chemicals == "all") {

    exposure_args <- list(location = as.character(exposure_mat$location),
                          radius = as.numeric(exposure_mat$radius),
                          start_date = as.character(exposure_mat$start_date),
                          end_date = as.character(exposure_mat$end_date))
    exposure_lists <- purrr::pmap(exposure_args, calculate_exposure,
                                  verbose = verbose,
                                  clean_pur_df = clean_pur_df)


  }


  if (write_plots = TRUE) {

    plot_exposure(exposure_list, ...)

  }



}

