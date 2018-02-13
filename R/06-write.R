
# ...  for plot_exposure() function
# not working for chemicals = "chemical_class" yet

write_exposure <- function(clean_pur_df, locations_dates_df, radii,
                           chemicals = "all", aerial_ground = FALSE,
                           directory, write_plots = TRUE, verbose = TRUE, ...) {

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
    radius_list[[i]] <- locations_dates_df %>% mutate(radius = radii[i])
  }
  exposure_mat <- do.call("rbind", radius_list)

  safe_calculate_exposure <- purrr::safely(calculate_exposure)

  exposure_args <- list(location = as.character(exposure_mat$location),
                        radius = as.numeric(exposure_mat$radius),
                        start_date = as.character(exposure_mat$start_date),
                        end_date = as.character(exposure_mat$end_date),
                        original_location = as.character(exposure_mat$original_location))
  exposure_list <- purrr::pmap(exposure_args, safe_calculate_exposure,
                                clean_pur_df = clean_pur_df,
                                chemicals = chemicals,
                                aerial_ground = aerial_ground,
                                verbose = verbose)

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
        dplyr::mutate(n_row = n())

      meta_data <- exposure_list[[i]]$result$meta_data %>%
        dplyr::mutate(error_message = NA,
                      location = exposure_mat[i, ]$original_location)

    }

    if (i == 1) {
      row_out <- row
    } else {
      row_out <- rbind(row_out, row)
    }

    if (i == 1) {
      for (l in 1:row$n_row) {
        meta_list[[l]] <- meta_data
      }
    } else {
      starting_point <- length(meta_list)
      meta_list_vec <- 1:row$n_row + starting_point
      for (l in meta_list_vec) {
        meta_list[[l]] <- meta_data
      }
    }

  }

  if (!dir.exists(directory)) {
    dir.create(directory)
  }

  row_out <- row_out %>% dplyr::select(-n_row)

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

        if (!is.null(plot_args$fill_option)) {
          fill_option <- plot_args$fill_option
        } else {
          fill_option <- "viridis"
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

        plot_list <- plot_exposure(exposure_list[[i]]$result, color_by = color_by,
                                   buffer_or_county = buffer_or_county,
                                   percentile = percentile,
                                   fill_option = fill_option, alpha = alpha,
                                   pls_labels = pls_labels,
                                   pls_labels_size = pls_labels_size)

        for (j in 1:length(plot_list$maps)) {

          exp_plot <- plot_list$maps[[j]]

          row_to_match <- plot_list$exposure[[j]] %>% dplyr::ungroup()

          row_out_noerror <- row_out %>% dplyr::select(-error_message) %>%
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

