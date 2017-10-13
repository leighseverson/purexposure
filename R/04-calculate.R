#' Calculate exposure to active ingredients present in applied pesticides.
#'
#' For a particular location, buffer radius, date range, and active ingredient
#' or class of active ingredients, \code{calculate_exposure} calculates an
#' estimate of exposure in kg of active ingredient per m^2.
#'
#' @param location A character string. Either a California address including
#'   street name, city, state, and zip code, or a pair of coordinates in the
#'   form "longitude, latitude".
#' @param clean_pur_df A data frame returned by \code{pull_clean_pur} that
#'   includes data for the county in which your location is located (before
#'   running \code{pull_clean_pur}, you can use the \code{find_location_county}
#'   function to figure this out), the time period, and the active ingredients
#'   or chemical classes for which you want to calculate exposure.
#' @param radius A numeric value greater than zero that gives the radius in meters
#'   defining the buffer around your location in which you would like to
#'   calculate exposure. For reference, the length and width of a PLS section is
#'   about 1,609 meters (1 mile). That of a township could range from about
#'   9,656 to 11,265 meters (6-7 miles).
#' @param time_period Optional. A character string giving a time period over which you
#'   would like to calculate exposure. For example, if you enter "6 months" for
#'   \code{time_period}, \code{calculate_exposure} will calculate exposure for
#'   every six month period starting from the earliest date present in the
#'   \code{clean_pur_df} data frame. Start and end dates can be optionally specified
#'   with the \code{start_date} and \code{end_date} arguments. Alternatively, to
#'   calculate exposure over only one time period, you can leave this argument
#'   NULL and specify start and end dates.
#' @param start_date Optional. "yyyy-mm-dd" specifying the start date for
#'   exposure estimation. This date should be present in the \code{clean_pur_df}
#'   data frame.
#' @param end_date Optional. "yyyy-mm-dd" specifying the end date for exposure
#'   estimation. This date should be present in the \code{clean_pur_df}
#'   data frame.
#' @param chemicals Either "all" or "chemical_class". The default is "all", which
#'   will calculate exposure to the summed active ingredients present in the
#'   \code{clean_pur_df} data frame. Enter "chemical_class" to calculate
#'   exposure to each of the chemical classes present in the \code{chemical_class}
#'   column of your \code{clean_pur_df} data frame.
#'
#' @return A list with four elements:
#'  \describe{
#'    \item{exposure}{A data frame with a row for each date
#'    range for which exposure was calculated and seven columns: \code{exposure},
#'    the estimate of exposure in kg/m^2, \code{location}, \code{start_date} and
#'    \code{end_date}, \code{radius}, the radius in meters for the buffer
#'    extending from the location, and \code{chemcials}, (either "all", indicating
#'    that all active ingredients present in the \code{clean_pur_df} were summed,
#'    a particular active ingredient, or the chemical class(es) specified in the
#'    \code{clean_pur_df} data frame). \code{none_recorded} is TRUE if exposure was
#'    0 because there was no pesticide application recorded in the sections or
#'    townships intersected by the buffer for that date range.}
#'    \item{meta_data}{A data frame with 11 columns and one row for every section
#'    or township intersected by the specified buffer extending from the
#'    given location. Columns include \code{location}, \code{section} or
#'    \code{township}, \code{percent}, the percent that the section is overlapped
#'    by the buffer, \code{kg}, the total amount of kg applied for the specified
#'    chemicals and date range in that section or township, \code{kg_int}, the
#'    amount of kilograms applied multiplied by the percent of overlap,
#'    \code{start_date} and \code{end_date}, \code{radius}, \code{chemicals},
#'    \code{area}, the area of the buffer (m^2), and \code{none_recorded}, logical
#'    for whether any pesticide application was recorded for the specified section
#'    or township, date range, and chemicals.}
#'    \item{buffer_plot}{A data frame with 24 columns. Contains spatial plotting
#'    data for the buffer and overlapping sections or townships. You can use the
#'    \code{plot_tidy} function to quickly plot and get a rough idea of the
#'    area for which exposure was calculated, before moving on to other map_*
#'    or plot_* functions.}
#'    \item{county_plot}{A ggplot2 plot showing the location of your specified
#'    buffer in the context of the county. Depending on if your \code{clean_pur_df}
#'    data frame was summed by section or township, the county will be shown
#'    with the relevant PLS units.}
#'  }
#'
#' @section Note:
#'  \itemize{
#'    \item{If the \code{time_period}, \code{start_date}, and \code{end_date}
#'          arguments are all left as NULL (their defaults), then exposure will
#'          be estiamted across the entire date range of the \code{clean_pur_df}
#'          data frame.}
#'    \item{If you pulled PUR data from \code{pull_clean_pur} specifying
#'          \code{sum_application = TRUE} and \code{unit = "township"}, then
#'          exposure will be calculated based on townships. Using the
#'          \code{plot_tidy} function to plot the returned \code{buffer_plot}
#'          list element and taking a look at the \code{county_plot} plot element
#'          could be helpful to see the difference between calculating exposure
#'          based on sections or townships for a certain buffer radius.}
#' }
#'
#' @examples
#' \dontrun{
#' clean_pur <- pull_clean_pur(2000, counties = "10")
#' exposure_list <- calculate_exposure(clean_pur,
#'                                     location = "13883 Lassen Ave, Helm, CA 93627",
#'                                     radius = 3000)
#' exposure_list$county_plot
#'
#' # specify time intervals
#' exp_list2 <- calculate_exposure(clean_pur,
#'                                 location = "13883 Lassen Ave, Helm, CA 93627",
#'                                 radius = 3000,
#'                                 time_period = "4 months")
#' exp_list2$exposure
#'
#' # calculate exposure by township
#' clean_pur2 <- pull_clean_pur(1995, counties = "san bernardino",
#'                              sum_application = TRUE, unit = "township")
#' exp_list3 <- calculate_exposure(clean_pur2,
#'                                 location = c("-116.45, 34.96")
#'                                 radius = 5000)
#' plot_tidy(exp_list3$buffer_plot)
#'
#' # calculate exposure by specified chemical classes
#' chemical_class_df <- rbind(find_chemical_codes(2000, "methylene"),
#'                            find_chemical_codes(2000, "aldehyde")) %>%
#'    dplyr::rename(chemical_class = chemical)
#' clean_pur3 <- pull_clean_pur(1995, "fresno",
#'                              chemicals = chemical_class_df$chemname,
#'                              sum_application = TRUE,
#'                              sum = "chemical_class",
#'                              chemical_class = chemical_class_df)
#' exp_list4 <- calculate_exposure(clean_pur3,
#'                                 location = "13883 Lassen Ave, Helm, CA 93627",
#'                                 radius = 1500,
#'                                 chemicals = "chemical_class")
#' }
#' @export
calculate_exposure <- function(clean_pur_df, location, radius,
                               time_period = NULL, start_date = NULL,
                               end_date = NULL, chemicals = "all") {

  # get numeric coordinate vector from location
  if (length(grep("-", location)) == 1) {
    latlon <- location
    latlon_vec <- as.numeric(as.vector(sapply(unlist(strsplit(latlon, ",")),
                                              stringr::str_trim)))
    address_x <- latlon_vec[1]
    address_y <- latlon_vec[2]
    latlon_out <- latlon_vec
  } else {
    address <- location
    suppressMessages(latlon_df <- ggmap::geocode(address, messaging = FALSE))
    address_x <- latlon_df$lon
    address_y <- latlon_df$lat
    latlon_out <- as.numeric(c(latlon_df$lon, latlon_df$lat))
  }

  # pull county shapefile
  county <- find_location_county(location, return = "name")
  check <- toupper(county) %in% clean_pur_df$county_name
  if (!check) {
    stop(paste0("\"", location,  "\"", " is located in ", county, " county. ",
                "\nThe clean_pur_df data frame doesn't include data for this ",
                "county."))
  }

  radius <- as.numeric(radius)

  # coordinates for buffer
  buffer <- geosphere::destPoint(p = c(address_x, address_y), b = 0:360,
                                 d = radius)
  colnames(buffer)[1] <- "long"
  buffer_df <- as.data.frame(buffer)
  range <- buffer_df %>% dplyr::summarise(min_long = min(long),
                                          min_lat = min(lat),
                                          max_long = max(long),
                                          max_lat = max(lat))

  if ("section" %in% colnames(clean_pur_df)) {
    shp <- pull_spdf(county, "section")
    df <- spdf_to_df(shp)
  } else {
    shp <- pull_spdf(county, "township")
    df <- spdf_to_df(shp)
  }

  context_plot <- ggplot(data = buffer_df) +
    geom_polygon(aes(x = long, y = lat), color = "red", fill = NA) +
    geom_polygon(data = df, aes(x = long, y = lat, group = group),
                 color = "black", fill = NA) +
    theme_void()

  # find sections (and townships) w/in buffer
  which_pls <- df %>% dplyr::filter(long >= range$min_long &
                                       long <= range$max_long &
                                       lat >= range$min_lat &
                                       lat <= range$max_lat)

  if (nrow(which_pls) == 0) {

    euc_distance <- function(long, lat, origin_long, origin_lat) {
      x <- abs(lat - origin_lat)
      y <- abs(long - origin_long)
      dist <- sqrt((x^2) + (y^2))
      out <- data.frame(long = long, lat = lat, dist = dist)
    }

    if ("section" %in% colnames(clean_pur_df)) {

      borders <- df %>% group_by(MTRS) %>%
        dplyr::summarise(min_long = min(long), min_lat = min(lat),
                         max_long = max(long), max_lat = max(lat))

      corner <- purrr::map2_dfr(borders$max_long, borders$max_lat, euc_distance,
                                origin_long = range$max_long,
                                origin_lat = range$max_lat) %>%
        dplyr::filter(long > range$max_long & lat > range$max_lat) %>%
        dplyr::filter(dist == min(dist))

      closest_pls <- borders %>% dplyr::filter(max_long == corner$long,
                                               max_lat == corner$lat) %>%
        dplyr::select(MTRS) %>%
        tibble_to_vector()

      which_pls <- df %>% dplyr::filter(MTRS == closest_pls)

    } else {

      borders <- df %>% group_by(MTR) %>%
        dplyr::summarise(min_long = min(long), min_lat = min(lat),
                         max_long = max(long), max_lat = max(lat))

      corner <- purrr::map2_dfr(borders$max_long, borders$max_lat, euc_distance,
                                origin_long = range$max_long,
                                origin_lat = range$max_lat) %>%
        dplyr::filter(long > range$max_long & lat > range$max_lat) %>%
        dplyr::filter(dist == min(dist))

      closest_pls <- borders %>% dplyr::filter(max_long == corner$long,
                                               max_lat == corner$lat) %>%
        dplyr::select(MTR) %>%
        tibble_to_vector()

      which_pls <- df %>% dplyr::filter(MTR == closest_pls)

    }
  }

  # data frame with start and end dates
  if (!is.null(time_period)) {
    # multiple time periods
    if (is.null(start_date)) {
      start_date <- min(clean_pur_df$date)
    } else {
      start_date <- lubridate::ymd(start_date)
    }
    if (is.null(end_date)) {
      end_date <- max(clean_pur_df$date)
    } else {
      end_date <- lubridate::ymd(end_date)
    }

    date_seq <- seq(start_date, end_date, by = time_period)
    end_dates <- dplyr::lead(date_seq, 1) - lubridate::days(1)

    time_df <- data.frame(start_date = date_seq,
                          end_date = end_dates)
    time_df$end_date[nrow(time_df)] <- end_date

  } else {

    # one time period
    if (is.null(start_date)) {
      start_date <- min(clean_pur_df$date)
    } else {
      start_date <- lubridate::ymd(start_date)
    }
    if (is.null(end_date)) {
      end_date <- max(clean_pur_df$date)
    } else {
      end_date <- lubridate::ymd(end_date)
    }

    time_df <- data.frame(start_date = start_date, end_date = end_date)

  }

  # # check_dates
  #
  # check_min_date <- min(time_df$start_date) == min(clean_pur_df$date)
  # check_max_date <- max(time_df$end_date) == max(clean_pur_df$date)
  #
  # if (!check_min_date | !check_max_date) {
  #   stop("Your pur_clean_df data frame doesn't contain data for the entire date\n ",
  #        "range speficied by start_date and end_date. You may need to pull data\n ",
  #        "for additional years using the pull_clean_pur() function." )
  # }

  if ("section" %in% colnames(clean_pur_df)) {

    mtrs_int <- as.character(unique(which_pls$MTRS))

    # filter shp file to include only sections intersecting w/ buffer
    df_filtered <- dplyr::filter(df, MTRS %in% mtrs_int)

    # combine buffer to existing sections shp file
    buffer_shp <- shp[1, ]
    buffer_shp@polygons[[1]]@Polygons[[1]]@coords <- buffer
    buffer_shp <- sp::spChFIDs(buffer_shp, paste("buffer", 1:nrow(buffer_shp), sep = ""))
    buffer_shp@data$MTRS <- "buffer"

    comb_shp <- maptools::spRbind(shp, buffer_shp)
    suppressMessages(comb_df <- broom::tidy(comb_shp))

    comb_shp_filt <- subset(comb_shp, MTRS %in% c(mtrs_int, "buffer"))

    buffer_poly <- length(comb_shp_filt@polygons)
    section_polys <- 1:(length(comb_shp_filt@polygons) - 1)

    for (i in 1:length(section_polys)) {

      section_area <- comb_shp_filt@polygons[[i]]@Polygons[[1]]@area
      intersection <- rgeos::gIntersection(sp::SpatialPolygons(comb_shp_filt@polygons[i]),
                                           sp::SpatialPolygons(comb_shp_filt@polygons[buffer_poly]))

      if (is.null(intersection)) {
        intersected_area <- 0
      } else {
        intersected_area <- rgeos::gArea(intersection)
      }

      percent_df <- data.frame(MTRS = comb_shp_filt@data$MTRS[i],
                               percent = intersected_area/section_area)

      if (i == 1) {
        out <- percent_df
      } else {
        out <- rbind(out, percent_df)
      }
    }

    out <- dplyr::filter(out, percent != 0)
    comb_shp_filt <- subset(comb_shp_filt, MTRS %in% c(as.character(out$MTRS),
                                                       "buffer"))
    suppressMessages(comb_df_filt <- broom::tidy(comb_shp_filt))

    comb_df_filt <- dplyr::mutate(comb_df_filt, group = as.character(group))

    comb_df_filt <- df_filtered %>%
      dplyr::mutate(group = as.character(group)) %>%
      dplyr::right_join(comb_df_filt, by = c("long", "lat", "hole", "order",
                                             "piece", "id", "group"))

    pur_filt <- dplyr::filter(clean_pur_df, !is.na(kg_chm_used))

    if (!any(unique(clean_pur_df$section) %in% mtrs_int)) {

      buffer_area <- pi * (radius^2)

      if (chemicals == "chemical_class") {
        exp <- out %>% dplyr::mutate(MTRS = as.character(MTRS),
                                     kg = 0,
                                     kg_int = 0,
                                     radius = radius,
                                     start_date = time_df$start_date,
                                     end_date = time_df$end_date,
                                     chemicals = unique(clean_pur_df$chemical_class),
                                     area = buffer_area,
                                     location = location,
                                     none_recorded = TRUE) %>%
          dplyr::rename(section = MTRS) %>%
          dplyr::select(location, section, percent, kg, kg_int, start_date,
                        end_date, radius, chemicals, area, none_recorded)
        row_out <- data.frame(exposure = 0,
                              location = location,
                              start_date = time_df$start_date,
                              end_date = time_df$end_date,
                              radius = radius,
                              chemicals = unique(clean_pur_df$chemical_class),
                              none_recorded = TRUE)
      } else {
        exp <- out %>% dplyr::mutate(MTRS = as.character(MTRS),
                                     kg = 0,
                                     kg_int = 0,
                                     radius = radius,
                                     start_date = time_df$start_date,
                                     end_date = time_df$end_date,
                                     chemicals = chemicals,
                                     area = buffer_area,
                                     location = location,
                                     none_recorded = TRUE) %>%
          dplyr::rename(section = MTRS) %>%
          dplyr::select(location, section, percent, kg, kg_int, start_date,
                        end_date, radius, chemicals, area, none_recorded)
        row_out <- data.frame(exposure = 0,
                              location = location,
                              start_date = time_df$start_date,
                              end_date = time_df$end_date,
                              radius = radius,
                              chemicals = chemicals,
                              none_recorded = TRUE)

      }

    } else {

      out <- purrr::map2(time_df$start_date, time_df$end_date,
                             calculate_exposure_bydate)
      for (i in 1:length(out)) {
        exp_row <- out[[i]]$row_out
        meta_data <- out[[i]]$meta_data[[1]]
        if (i == 1) {
          row_out <- exp_row
          meta_out <- meta_data
        } else {
          row_out <- rbind(row_out, exp_row)
          meta_out <- rbind(meta_out, meta_data)
        }
      }

      out <- list(exposure = row_out,
                  meta_data = meta_out)

    }

  } else {

    # townships

    mtrs_int <- as.character(unique(which_pls$MTR))

    # filter shp file to include only sections intersecting w/ buffer
    df_filtered <- dplyr::filter(df, MTR %in% mtrs_int)

    # combine buffer to existing sections shp file
    buffer_shp <- shp[1, ]
    buffer_shp@polygons[[1]]@Polygons[[1]]@coords <- buffer
    buffer_shp <- sp::spChFIDs(buffer_shp, paste("buffer", 1:nrow(buffer_shp), sep = ""))
    buffer_shp@data$MTR <- "buffer"

    comb_shp <- maptools::spRbind(shp, buffer_shp)
    suppressMessages(comb_df <- broom::tidy(comb_shp))

    comb_shp_filt <- subset(comb_shp, MTR %in% c(mtrs_int, "buffer"))

    buffer_poly <- length(comb_shp_filt@polygons)
    section_polys <- 1:(length(comb_shp_filt@polygons) - 1)

    for (i in 1:length(section_polys)) {

      section_area <- comb_shp_filt@polygons[[i]]@Polygons[[1]]@area
      intersection <- rgeos::gIntersection(sp::SpatialPolygons(comb_shp_filt@polygons[i]),
                                           sp::SpatialPolygons(comb_shp_filt@polygons[buffer_poly]))

      if (is.null(intersection)) {
        intersected_area <- 0
      } else {
        intersected_area <- rgeos::gArea(intersection)
      }

      percent_df <- data.frame(MTR = comb_shp_filt@data$MTR[i],
                               percent = intersected_area/section_area)

      if (i == 1) {
        out <- percent_df
      } else {
        out <- rbind(out, percent_df)
      }
    }

    out <- dplyr::filter(out, percent != 0)
    comb_shp_filt <- subset(comb_shp_filt, MTR %in% c(as.character(out$MTR),
                                                       "buffer"))
    suppressMessages(comb_df_filt <- broom::tidy(comb_shp_filt))

    comb_df_filt <- dplyr::mutate(comb_df_filt, group = as.character(group))

    comb_df_filt <- df_filtered %>%
      dplyr::mutate(group = as.character(group)) %>%
      dplyr::right_join(comb_df_filt, by = c("long", "lat", "hole", "order",
                                             "piece", "id", "group"))

    pur_filt <- dplyr::filter(clean_pur_df, !is.na(kg_chm_used))

    if (!any(unique(clean_pur_df$township) %in% mtrs_int)) {

      buffer_area <- pi * (radius^2)

      exp <- out %>% dplyr::mutate(MTR = as.character(MTR),
                                   kg = 0,
                                   kg_int = 0,
                                   radius = radius,
                                   start_date = time_df$start_date,
                                   end_date = time_df$end_date,
                                   chemicals = chemicals,
                                   area = buffer_area,
                                   location = location,
                                   none_recorded = TRUE) %>%
        dplyr::rename(township = MTR) %>%
        dplyr::select(location, township, percent, kg, kg_int, start_date,
                      end_date, radius, chemicals, area, none_recorded)

      row_out <- data.frame(exposure = 0,
                            location = location,
                            start_date = time_df$start_date,
                            end_date = time_df$end_date,
                            radius = radius,
                            chemicals = chemicals,
                            none_recorded = TRUE)


    } else {

      out <- purrr::map2(time_df$start_date, time_df$end_date,
                         calculate_exposure_bydate)
      for (i in 1:length(out)) {
        exp_row <- out[[i]]$row_out
        meta_data <- out[[i]]$meta_data[[1]]
        if (i == 1) {
          row_out <- exp_row
          meta_out <- meta_data
        } else {
          row_out <- rbind(row_out, exp_row)
          meta_out <- rbind(meta_out, meta_data)
        }
      }

      out <- list(exposure = row_out,
                  meta_data = meta_out,
                  buffer_plot = comb_df_filt,
                  county_plot = context_plot)

    }

  }

  return(out)

}

#' Calculate exposure for multiple start and end dates
#'
#' \code{calculate_exposure_bydate} takes a start and end date and returns
#' exposure information. Written to be used in the \code{calculate_exposure}
#' function- has dependencies for other variables (pur_filt data frame, radius,
#' location, and chemicals) produced there.
#'
#' @param start_date "yyyy-mm-dd"
#' @param end_date "yyyy-mm-dd"
#'
#' @return A data frame with two columns. \code{row_out} is a nested data frame
#' with one row and seven columns. \code{meta_data} is a nested list with one
#' element: a data frame with as many rows as there are sections or townships
#' intersecting with the specified buffer and 11 columns.
calculate_exposure_bydate <- function(start_date, end_date) {

  if (chemicals == "all") { #all

    if ("section" %in% colnames(pur_filt)) {

      pur_out <- pur_filt %>%
        dplyr::filter(date >= start_date & date <= end_date) %>%
        dplyr::group_by(section) %>%
        dplyr::summarise(kg = sum(kg_chm_used)) %>%
        dplyr::ungroup()

      buffer_area <- pi * (radius^2)

      exp <- out %>%
        dplyr::mutate(MTRS = as.character(MTRS)) %>%
        dplyr::rename(section = MTRS) %>%
        dplyr::left_join(pur_out, by = "section") %>%
        dplyr::mutate(kg_int = percent * kg,
                      location = location,
                      radius = radius,
                      start_date = start_date,
                      end_date = end_date,
                      chemicals = chemicals,
                      area = buffer_area,
                      none_recorded = FALSE) %>%
        dplyr::select(location, section, percent, kg, kg_int,
                      start_date, end_date, radius, chemicals, area,
                      none_recorded)

      exp_out <- exp %>% dplyr::summarise(exposure =
                                            sum(kg_int, na.rm = TRUE) /
                                            buffer_area)

      row_out <- data.frame(exposure = exp_out, location = location,
                            start_date = start_date,  end_date = end_date,
                            radius = radius, chemicals = chemicals,
                            none_recorded = FALSE)

    } else {

      pur_out <- pur_filt %>%
        dplyr::filter(date >= start_date & date <= end_date) %>%
        dplyr::group_by(township) %>%
        dplyr::summarise(kg = sum(kg_chm_used)) %>%
        dplyr::ungroup()

      buffer_area <- pi * (radius^2)

      exp <- out %>%
        dplyr::mutate(MTRS = as.character(MTRS)) %>%
        dplyr::rename(township = MTRS) %>%
        dplyr::left_join(pur_out, by = "township") %>%
        dplyr::mutate(kg_int = percent * kg,
                      location = location,
                      radius = radius,
                      start_date = start_date,
                      end_date = end_date,
                      chemicals = chemicals,
                      area = buffer_area,
                      none_recorded = FALSE) %>%
        dplyr::select(location, township, percent, kg, kg_int,
                      start_date, end_date, radius, chemicals, area,
                      none_recorded)

      exp_out <- exp %>% dplyr::summarise(exposure =
                                            sum(kg_int, na.rm = TRUE) /
                                            buffer_area)

      row_out <- data.frame(exposure = exp_out, location = location,
                            start_date = start_date,  end_date = end_date,
                            radius = radius, chemicals = chemicals,
                            none_recorded = FALSE)


    }

  } else if (chemicals == "chemical_class") { #chemical_class

    if ("section" %in% colnames(pur_filt)) {

      pur_out <- pur_filt %>%
        dplyr::filter(date >= start_date & date <= end_date) %>%
        dplyr::group_by(chemical_class, section) %>%
        dplyr::summarise(kg = sum(kg_chm_used)) %>%
        dplyr::ungroup()

      buffer_area <- pi * (radius^2)

      exp <- out %>%
        dplyr::mutate(MTRS = as.character(MTRS)) %>%
        dplyr::rename(section = MTRS) %>%
        dplyr::left_join(pur_out, by = "section") %>%
        dplyr::mutate(kg_int = percent * kg,
                      location = location,
                      radius = radius,
                      start_date = start_date,
                      end_date = end_date,
                      chemicals = chemical_class,
                      area = buffer_area,
                      none_recorded = FALSE) %>%
        dplyr::select(location, section, percent, kg, kg_int,
                      start_date, end_date, radius, chemicals, area,
                      none_recorded)

      test_vec <- unique(clean_pur_df$chemical_class) %in% unique(exp$chemicals)
      test <- all(test_vec)
      if(!test) {
        missing_classes <- unique(clean_pur_df$chemical_class)[!test_vec]
        df_to_add <- data.frame(location = rep(location, length(missing_classes)),
                                section = rep(NA, length(missing_classes)),
                                percent = rep(NA, length(missing_classes)),
                                kg = rep(0, length(missing_classes)),
                                kg_int = rep(NA, length(missing_classes)),
                                start_date = rep(start_date, length(missing_classes)),
                                end_date = rep(end_date, length(missing_classes)),
                                radius = rep(radius, length(missing_classes)),
                                chemicals = missing_classes,
                                area = rep(buffer_area, length(missing_classes)),
                                none_recorded = rep(none_recorded, length(missing_classes)))
        exp <- rbind(exp, df_to_add)

      }

      exp_out <- exp %>% dplyr::group_by(chemicals) %>%
        dplyr::summarise(exposure = sum(kg_int, na.rm = TRUE) / buffer_area)

      to_join <- exp %>% dplyr::select(chemicals, none_recorded)

      row_out <- data.frame(exposure = exp_out$exposure,
                            location = rep(location, nrow(exp_out)),
                            start_date = rep(start_date, nrow(exp_out)),
                            end_date = rep(end_date, nrow(exp_out)),
                            radius = rep(radius, nrow(exp_out)),
                            chemicals = exp_out$chemicals) %>%
        dplyr::mutate(chemicals = as.character(chemicals),
                      location = as.factor(location)) %>%
        dplyr::full_join(to_join, by = "chemicals")


    } else {

      pur_out <- pur_filt %>%
        dplyr::filter(date >= start_date & date <= end_date) %>%
        dplyr::group_by(chemical_class, township) %>%
        dplyr::summarise(kg = sum(kg_chm_used)) %>%
        dplyr::ungroup()

      buffer_area <- pi * (radius^2)

      exp <- out %>%
        dplyr::mutate(MTR = as.character(MTR)) %>%
        dplyr::rename(township = MTR) %>%
        dplyr::left_join(pur_out, by = "township") %>%
        dplyr::mutate(kg_int = percent * kg,
                      location = location,
                      radius = radius,
                      start_date = start_date,
                      end_date = end_date,
                      chemicals = chemical_class,
                      area = buffer_area,
                      none_recorded = FALSE) %>%
        dplyr::select(location, township, percent, kg, kg_int,
                      start_date, end_date, radius, chemicals, area,
                      none_recorded)

      test_vec <- unique(clean_pur_df$chemical_class) %in% unique(exp$chemicals)
      test <- all(test_vec)
      if(!test) {
        missing_classes <- unique(clean_pur_df$chemical_class)[!test_vec]
        df_to_add <- data.frame(location = rep(location, length(missing_classes)),
                                township = rep(NA, length(missing_classes)),
                                percent = rep(NA, length(missing_classes)),
                                kg = rep(0, length(missing_classes)),
                                kg_int = rep(NA, length(missing_classes)),
                                start_date = rep(start_date, length(missing_classes)),
                                end_date = rep(end_date, length(missing_classes)),
                                radius = rep(radius, length(missing_classes)),
                                chemicals = missing_classes,
                                area = rep(buffer_area, length(missing_classes)),
                                none_recorded = rep(TRUE, length(missing_classes)))
        exp <- rbind(exp, df_to_add)

      }

      exp_out <- exp %>% dplyr::group_by(chemicals) %>%
        dplyr::summarise(exposure = sum(kg_int, na.rm = TRUE) / buffer_area)

      to_join <- exp %>% dplyr::select(chemicals, none_recorded)

      row_out <- data.frame(exposure = exp_out$exposure,
                            location = rep(location, nrow(exp_out)),
                            start_date = rep(start_date, nrow(exp_out)),
                            end_date = rep(end_date, nrow(exp_out)),
                            radius = rep(radius, nrow(exp_out)),
                            chemicals = exp_out$chemicals) %>%
        dplyr::mutate(chemicals = as.character(chemicals),
                      location = as.factor(location)) %>%
        dplyr::full_join(to_join, by = "chemicals")

    }
  }

  nested_df <- list(row_out = row_out, meta_data = list(exp))
  attr(nested_df, "row.names") <- as.integer(1)
  class(nested_df) <- c("tbl_df", "data.frame")

  return(nested_df)

}
