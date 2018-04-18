#' Write exposure values for specified locations and dates in a certain county.
#'
#' For a particular combination of locations and dates, radii, and active
#' ingredients, \code{write_exposure} calculates exposure (kg/m^2) and writes
#' out files (exposure values, meta data, and plots) to a specified directory.
#'
#' @param clean_pur_df A data frame returned by \code{pull_clean_pur} that
#'   includes data for the county of your locations, the time periods, and the
#'   active ingredients or cheimcal classes for which you want to calculate
#'   exposure.
#' @param locations_dates_df A data frame with three columns: \code{location},
#'   character strings of California addresses with street names, cities, state,
#'   and 5-digit zip codes, or pairs of coordinates in the form "longitude,
#'   latitude". All of the locations should be in the same county. The other two
#'   columns are \code{start_date} and \code{end_date}, with "yyyy-mm-dd"
#'   character strings specifying the time period(s) over which you would like
#'   to calculate exposure for each location.
#' @param radii A vector of numeric values greater than zero that give the radii
#'   in meters defining the buffers around your locations in which you would
#'   like to calculate exposure. For reference, the length and width of a PLS
#'   section is about 1,609 meters (1 mile). That of a township could range from
#'   about 9,656 to 11,265 meters (6-7 miles).
#' @param directory A path to the directory where you would like exposure files
#'   to be written. This directory will be created if it doesn't already exist.
#' @param write_plots TRUE / FALSE for whether you would like to write out
#'   plots returned from \code{plot_exposure} for each combination of location,
#'   date, radius, chemical class, and aerial/ground application. Plots are
#'   saved in a subdirectory called "exposure_plots".
#' @param ... Additional arguments passed on to \code{plot_exposure}.
#' @inheritParams calculate_exposure
#'
#' @return Two .rds files ("exposure_df" and "meta_data") and a subdirectory
#'   ("exposure_plots") with PNG files of \code{plot_exposure} plots:
#' \describe{
#'   \item{exposure_df.rds}{A data frame with 10 columns: \code{exposure}, the
#'   exposure value in kg/m^2 for that combination of chemicals, date range,
#'   aerial/ground application, location, and radius, \code{chemicals}, either
#'   "all" or a chemical class present in the provided \code{clean_pur_df} data
#'   frame, \code{start_date} and \code{end_date}, \code{aerial_ground}, which
#'   will be \code{NA} unless the \code{aerial_ground} argument is set to
#'   \code{TRUE}, \code{location}, \code{radius}, \code{longitude} and
#'   \code{latitude} of each location, and any \code{error_message}s that were
#'   returned when calculating exposure.}
#'   \item{meta_data.rds}{A list with as many elements as there are rows in the
#'   exposure_df.rds data frame. Each element is a data frame with meta data
#'   relevant to the exposure value in the corresponding row of the exposure_df
#'   data frame. For example, meta data for exposure_df[1,] is saved as
#'   meta_data[[1]], exposure_df[2,] saved as meta_data[[2]], and so on. Meta
#'   data data frames have 13 columns: \code{pls}, with each PLS unit intersected
#'   by the specified buffer, \code{chemicals}, either a chemical class or "all"
#'   (indicating exposure was calculated for all active ingredients present in
#'   the clean_pur_df data frame), \code{percent}, the percent intersection of
#'   the PLS unit with the buffer, \code{kg}, kg of active ingredients applied
#'   in the PLS unit for the given date range/chemicals/aerial_ground combination,
#'   \code{kg_intersection}, \code{percent} multiplied by \code{kg},
#'   \code{start_date}, \code{end_date}, \code{aerial_ground} (this will be
#'   \code{NA} if the \code{aerial_ground} argument is set to \code{FALSE} or if
#'   \code{none_recorded} is \code{TRUE}), \code{none_recorded}, a logical value
#'   indicating if there were no active ingredients recorded for the
#'   PLS/date range/chemicals combination, \code{location}, \code{radius},
#'   \code{area}, the area of the specified buffer, and \code{error_message},
#'   which gives the error message, if any, that was returned.}
#'   \item{exposure_plots}{A subdirectory with a \code{plot_exposure} plot saved
#'   for each row of the exposure_df data frame and element of the meta_data list.
#'   Plots are saved as #_exposure_plot.png, with numbers corresponding to the
#'   row number and element number of the exposure_df data frame and meta_data
#'   list, respectively. For example, 12_exposure_plot.png corresponds to
#'   exposure_df[12,] and meta_data[[12]].}
#' }
#'
#' @section Note:
#'  \itemize{
#'    \item{Unlike the \code{calculate_exposure} function, \code{write_exposure}
#'          requires that you specify at least one start and end date for each
#'          location with the \code{locations_dates_df} argument. This is to
#'          accomodate multiple date ranges within a single location and
#'          differing start/end dates across locations.}
#' }
#'
#' @examples
#' library(magrittr)
#' \donttest{
#' chemical_class_df <- rbind(find_chemical_codes(2000:2001, "sulfur"),
#'                            find_chemical_codes(2000:2001, "methyl bromide")) %>%
#'    dplyr::rename(chemical_class = chemical)
#' pur <- pull_clean_pur(2000:2001, counties = "fresno",
#'                       chemicals = chemical_class_df$chemname,
#'                       sum_application = TRUE,
#'                       sum = "chemical_class",
#'                       chemical_class = chemical_class_df)
#' schools <- c("3333 American Ave., Fresno, CA 93725",
#'              "1111 Van Ness Ave., Fresno, CA 93721",
#'              "1616 South Fruit Ave., Fresno, CA 93706")
#' df <- data.frame(location = rep(schools, each = 2),
#'                  start_date = rep(c("2000-01-01", "2000-05-25", "2001-02-16"),
#'                                   each = 2),
#'                  end_date = c("2000-04-01", "2000-07-01",
#'                               "2000-08-25", "2000-11-25",
#'                               "2001-05-16", "2001-08-16"))
#' temp_dir <- tempdir()
#' write_exposure(pur, df, c(1500, 3000), "chemical_class",
#'                directory = temp_dir)
#' exposure_df <- readRDS(paste0(temp_dir, "/exposure_df.rds"))
#' meta_data <- readRDS(paste0(temp_dir, "/meta_data.rds"))
#' list.files(paste0(temp_dir, "/exposure_plots"))
#' }
#' \dontshow{
#' spdf <- readRDS(system.file("extdata", "fresno_spdf.rds", package = "purexposure"))
#' pur <- readRDS(system.file("extdata", "fresno_clean.rds", package = "purexposure"))
#' df <- data.frame(location = "-119.726751, 36.660967",
#'                  start_date = "2000-01-01",
#'                  end_date = "2000-12-31")
#' temp <- tempdir()
#' write_exposure(pur, df, 3000, temp, spdf = spdf)
#' }
#' @importFrom magrittr %>%
#' @export
write_exposure <- function(clean_pur_df, locations_dates_df, radii, directory,
                           chemicals = "all", aerial_ground = FALSE,
                           write_plots = TRUE, verbose = TRUE, ...) {

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
      latlon_df <- help_geocode(address)
      address_x <- latlon_df$lon
      address_y <- latlon_df$lat
      latlon_out <- as.numeric(c(address_x, address_y))
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

  locations_dates_df <- locations_dates_df %>%
    dplyr::full_join(loc_df, by = "location") %>%
    dplyr::rename(original_location = location,
                  location = latlon_loc)

  radius_list <- list()
  for (i in 1:length(radii)) {
    radius_list[[i]] <- locations_dates_df %>% dplyr::mutate(radius = radii[i])
  }
  exposure_mat <- do.call("rbind", radius_list)

  safe_calculate_exposure <- purrr::safely(calculate_exposure)

  exposure_args <- list(location = as.character(exposure_mat$location),
                        radius = as.numeric(exposure_mat$radius),
                        start_date = as.character(exposure_mat$start_date),
                        end_date = as.character(exposure_mat$end_date),
                        original_location = as.character(exposure_mat$original_location))

  args <- list(...)
  if (is.null(args$spdf)) {
    exposure_list <- purrr::pmap(exposure_args, safe_calculate_exposure,
                                 clean_pur_df = clean_pur_df, chemicals = chemicals,
                                 aerial_ground = aerial_ground, verbose = verbose)
  } else {
    exposure_list <- purrr::pmap(exposure_args, safe_calculate_exposure,
                                 clean_pur_df = clean_pur_df, chemicals = chemicals,
                                 aerial_ground = aerial_ground, verbose = verbose,
                                 spdf = spdf)
  }


  meta_list <- list()

  for (i in 1:length(exposure_list)) {

    if (!is.null(exposure_list[[i]]$error)) {

      row <- data.frame(exposure = NA, chemicals = chemicals,
                        start_date = exposure_mat[i, ]$start_date,
                        end_date = exposure_mat[i, ]$end_date,
                        aerial_ground = NA,
                        location = exposure_mat[i, ]$original_location,
                        radius = exposure_mat[i, ]$radius,
                        longitude = NA, latitude = NA,
                        error_message = exposure_list[[i]]$error,
                        n_row = 1)
      meta_data <- data.frame(pls = NA, chemicals = chemicals, percent = NA,
                              kg = NA, kg_intersection = NA,
                              start_date = exposure_mat[i, ]$start_date,
                              end_date = exposure_mat[i, ]$end_date,
                              aerial_ground = NA, none_recorded = NA,
                              location = exposure_mat[i, ]$original_location,
                              radius = exposure_mat[i, ]$radius,
                              area = NA, error_message = exposure_list[[i]]$error)

    } else {

      error_message <- NA
      row <- exposure_list[[i]]$result$exposure %>%
        dplyr::mutate(error_message = NA) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n_row = n())

      meta_data <- exposure_list[[i]]$result$meta_data %>%
        dplyr::mutate(error_message = NA,
                      location = exposure_mat[i, ]$original_location)

    }

    if (i == 1) {

      row_out <- row

      for (l in 1:unique(row$n_row)) {

        meta_row <- row[l,]

        chemicals_match <- meta_row$chemicals
        aerial_ground_match <- meta_row$aerial_ground
        radius_match <- meta_row$radius

        if (length(unique(meta_data$chemicals)) > 1) {
          meta_data_row <- meta_data %>%
            dplyr::filter(chemicals == chemicals_match)
          if (class(aerial_ground_match) != "logical" & aerial_ground_match %in%
              c("A", "G", "O")) {
            meta_data_row <- meta_data_row %>%
              dplyr::filter(aerial_ground == aerial_ground_match)
            if (length(unique(meta_data_row$radius)) > 1) {
              meta_data_row <- meta_data_row %>%
                dplyr::filter(radius == radius_match)
            }
          } else {

            if (length(unique(meta_data_row$radius)) > 1) {
              meta_data_row <- meta_data_row %>%
                dplyr::filter(radius == radius_match)
            }

          }
        } else {

          if (class(aerial_ground_match) != "logical" & aerial_ground_match %in%
              c("A", "G", "O")) {
            meta_data_row <- meta_data %>%
              dplyr::filter(aerial_ground == aerial_ground_match)

            if (length(unique(meta_data_row$radius)) > 1) {
              meta_data_row <- meta_data_row %>%
                dplyr::filter(radius == radius_match)
            }

          } else {

            if (length(unique(meta_data$radius)) > 1) {
              meta_data_row <- meta_data %>%
                dplyr::filter(radius == radius_match)
            }

          }

        }

        if (!exists("meta_data_row")) {
          meta_data_row <- meta_data
        }
        meta_list[[l]] <- meta_data_row

      }

    } else {

      row_out <- rbind(row_out, row)

      starting_point <- length(meta_list)
      meta_list_vec <- (1:unique(row$n_row)) + starting_point

      for (l in meta_list_vec) {

        meta_row <- row[l-starting_point,]

        chemicals_match <- meta_row$chemicals
        aerial_ground_match <- meta_row$aerial_ground
        radius_match <- meta_row$radius

        if (length(unique(meta_data$chemicals)) > 1) {
          meta_data_row <- meta_data %>%
            dplyr::filter(chemicals == chemicals_match)
          if (class(aerial_ground_match) != "logical" & aerial_ground_match %in%
              c("A", "G", "O")) {
            meta_data_row <- meta_data_row %>%
              dplyr::filter(aerial_ground == aerial_ground_match)
            if (length(unique(meta_data_row$radius)) > 1) {
              meta_data_row <- meta_data_row %>%
                dplyr::filter(radius == radius_match)
            }
          } else {

            if (length(unique(meta_data_row$radius)) > 1) {
              meta_data_row <- meta_data_row %>%
                dplyr::filter(radius == radius_match)
            }

          }
        } else {

          if (class(aerial_ground_match) != "logical" & aerial_ground_match %in%
              c("A", "G", "O")) {
            meta_data_row <- meta_data %>%
              dplyr::filter(aerial_ground == aerial_ground_match)

            if (length(unique(meta_data_row$radius)) > 1) {
              meta_data_row <- meta_data_row %>%
                dplyr::filter(radius == radius_match)
            }

          } else {

            if (length(unique(meta_data$radius)) > 1) {
              meta_data_row <- meta_data %>%
                dplyr::filter(radius == radius_match)
            }

          }

        }

        if (!exists("meta_data_row")) {
          meta_data_row <- meta_data
        }

        meta_list[[l]] <- meta_data_row

      }

    }

  }

  if (!dir.exists(directory)) {
    dir.create(directory)
  }

  row_out <- row_out %>%
    dplyr::select(-n_row) %>%
    dplyr::mutate(aerial_ground = ifelse(aerial_ground == "NA", NA, aerial_ground),
                  chemicals = ifelse(chemicals == "NA", NA, chemicals))

  saveRDS(row_out, file = paste0(directory, "/exposure_df.rds"))

  saveRDS(meta_list, file = paste0(directory, "/meta_data.rds"))

  pad_width <- nchar(sub('^0+','',sub('\\.', '', nrow(row_out))))

  if (write_plots) {

    if (!dir.exists(paste0(directory, "/exposure_plots"))) {
      dir.create(paste0(directory, "/exposure_plots"))
    }

    for (i in 1:length(exposure_list)) {

      if (is.null(exposure_list[[i]]$error)) {

        plot_args <- list(...)
        if (!is.null(plot_args$color_by)) {
          color_by <- plot_args$color_by
        } else {
          color_by <- "amount"
        }

        if (!is.null(plot_args$buffer_or_county)) {
          buffer_or_county <- plot_args$buffer_or_county
        } else {
          buffer_or_county <- "county"
        }

        if (!is.null(plot_args$percentile)) {
          percentile <- plot_args$percentile
        } else {
          percentile <- c(0.25, 0.5, 0.75)
        }

        if (!is.null(plot_args$fill)) {
          fill <- plot_args$fill
        } else {
          fill <- "viridis"
        }

        if (!is.null(plot_args$alpha)) {
          alpha <- plot_args$alpha
        } else {
          alpha <- 0.7
        }

        if (!is.null(plot_args$pls_labels)) {
          pls_labels <- plot_args$pls_labels
        } else {
          pls_labels <- FALSE
        }

        if (!is.null(plot_args$pls_labels_size)) {
          pls_labels_size <- plot_args$pls_labels_size
        } else {
          pls_labels_size <- 4
        }

        exposure_list[[i]]$result$exposure$chemicals <-
          as.character(exposure_list[[i]]$result$exposure$chemicals)

        exposure_list[[i]]$result$exposure$aerial_ground <-
          as.character(exposure_list[[i]]$result$exposure$aerial_ground)

        plot_list <- plot_exposure(exposure_list[[i]]$result, color_by = color_by,
                                   buffer_or_county = buffer_or_county,
                                   percentile = percentile,
                                   fill = fill, alpha = alpha,
                                   pls_labels = pls_labels,
                                   pls_labels_size = pls_labels_size)

        for (j in 1:length(plot_list$maps)) {
          exp_plot <- plot_list$maps[[j]]

          row_to_match <- plot_list$exposure[[j]] %>%
            dplyr::ungroup() %>%
            dplyr::mutate(aerial_ground = ifelse(aerial_ground == "NA", NA,
                                                 aerial_ground),
                          chemicals = as.character(chemicals))

          row_out_noerror <- row_out %>%
            dplyr::select(-error_message) %>%
            dplyr::ungroup()
         #  k <- which(apply(row_out_noerror, 1, identical, row_to_match))

          for (z in 1:nrow(row_out_noerror)) {
            match <- identical(row_out_noerror[z,], row_to_match)
            if (match) {
              k <- z
            }
          }

          file_number <- stringr::str_pad(as.character(k), pad_width, "left", pad = "0")

          ggplot2::ggsave(paste0(directory, "/exposure_plots/", file_number,
                                 "_exposure_plot.png"),
                          plot = exp_plot)

          if (!is.null(plot_list$cutoff_values[[j]])) {
            if (!dir.exists(paste0(directory, "/exposure_plots/cutoff_values"))) {
              dir.create(paste0(directory, "/exposure_plots/cutoff_values"))
            }
            cutoff_df <- plot_list$cutoff_values[[j]]
            saveRDS(cutoff_df, file = paste0(directory, "/exposure_plots/cutoff_values/",
                                             file_number, "_cutoff_values.rds"))
          }

        }

      }

    }

  }

}

