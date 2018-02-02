#' Calculate exposure to active ingredients present in applied pesticides.
#'
#' For a particular location, buffer radius, date range, and active ingredient
#' or class of active ingredients, \code{calculate_exposure} calculates an
#' estimate of exposure in kg of active ingredient per m^2.
#'
#' @param location A length-one character string. Either a California address
#'   including street name, city, state, and 5-digit zip code, or a pair of
#'   coordinates in the form "longitude, latitude".
#' @param clean_pur_df A data frame returned by \code{pull_clean_pur} that
#'   includes data for the county of your location (before running
#'   \code{pull_clean_pur}, you can use the
#'   \code{find_location_county} function to figure this out), the time period,
#'   and the active ingredients or chemical classes for which you want to
#'   calculate exposure.
#' @param radius A numeric value greater than zero that gives the radius in meters
#'   defining the buffer around your location in which you would like to
#'   calculate exposure. For reference, the length and width of a PLS section is
#'   about 1,609 meters (1 mile). That of a township could range from about
#'   9,656 to 11,265 meters (6-7 miles).
#' @param time_period Optional. A character string giving a time period over which you
#'   would like to calculate exposure in days, weeks, months, or years.
#'   For example, if you enter "6 months" for \code{time_period},
#'   \code{calculate_exposure} will calculate exposure for every six month
#'   period starting January 1 of the earliest year present in the
#'   \code{clean_pur_df} data frame. Start and end dates can be optionally specified
#'   with the \code{start_date} and \code{end_date} arguments. Alternatively, to
#'   calculate exposure over only one specified time period, you can leave this
#'   argument NULL and specify start and end dates.
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
#' @param aerial_ground TRUE / FALSE for whether you would like to
#'   incorporate aerial/ground application into exposure calculations. If
#'   \code{aerial_ground = TRUE}, there should be an \code{aerial_ground}
#'   column in the input \code{clean_pur_df} data frame. There will be a value of
#'   exposure calculated for each chemical ("all" or by chemical class) and for
#'   each method of application: aerial or ground. The default is FALSE.
#' @param verbose TRUE / FALSE for whether you would like a message to print out
#'   while the function is running. The default is \code{TRUE}.
#'
#' @return A list with five elements:
#'  \describe{
#'    \item{exposure}{A data frame with 9 columns: \code{exposure},
#'    the estimate of exposure in kg/m^2, \code{chemicals}, (either "all",
#'    indicating that all active ingredients present in the \code{clean_pur_df}
#'    were summed or the chemical class(es) specified in the \code{clean_pur_df}
#'    data frame), \code{start_date}, \code{end_date}, \code{aerial_ground},
#'    which can take values of "A" = aerial, "G" = ground, and "O" = others, (if
#'    the \code{aerial_ground} argument is \code{FALSE}, \code{aerial_ground}
#'    will be \code{NA} in the \code{exposure} data frame), \code{location},
#'    \code{radius}, the radius in meters for the buffer extending from the
#'    location, and the \code{longitude} and \code{latitude} of the location.}
#'    \item{meta_data}{A data frame with 12 columns and at least one row for
#'    every section or township intersected by the specified buffer extending
#'    from the given location. Columns include \code{pls}, giving either the
#'    Public Land Survey (PLS) section (9 characters long) or township (7
#'    characters long), \code{chemicals}, \code{percent}, the percent that the
#'    PLS unit is overlapped by the buffer, \code{kg}, the total amount of kg
#'    applied for the specified chemicals and date range in that section or
#'    township, \code{kg_intersection}, the amount of kilograms applied
#'    multiplied by the percent of overlap, \code{start_date} and \code{end_date},
#'    \code{aerial_ground}, which can take values of "A" (aerial), "G" (ground),
#'    or "O" (other), and will be \code{NA} if exposure calculations did not
#'    take aerial/ground application into account, \code{none_recorded}, logical
#'    for whether any pesticide application was recorded for the specified section
#'    or township, date range, and chemicals, \code{location}, and \code{radius}.}
#'    \item{buffer_plot_df}{A data frame with 24 columns. Contains spatial plotting
#'    data for the buffer and overlapping sections or townships. You can use the
#'    \code{df_plot} function to quickly plot and get a rough idea of the
#'    area for which exposure was calculated, before moving on to other
#'    plot_* functions.}
#'    \item{county_plot}{A ggplot2 plot showing the location of your specified
#'    buffer in the context of the county. Depending on if your \code{clean_pur_df}
#'    data frame was summed by section or township, the county will be shown
#'    with the relevant PLS units.}
#'    \item{clean_pur_df}{The data frame supplied to the \code{clean_pur_df}
#'    argument, filtered to the county and date range for which exposure
#'    was calculated.}
#'  }
#'
#' @section Note:
#'  \itemize{
#'    \item{If the \code{time_period}, \code{start_date}, and \code{end_date}
#'          arguments are all left as NULL (their defaults), then exposure will
#'          be estimated across the entire date range of the \code{clean_pur_df}
#'          data frame.}
#'    \item{If you pulled PUR data from \code{pull_clean_pur} specifying
#'          \code{sum_application = TRUE} and \code{unit = "township"}, then
#'          exposure will be calculated based on townships. Using the
#'          \code{df_plot} function to plot the returned \code{buffer_plot}
#'          list element could be helpful to see the difference between
#'          calculating exposure based on sections or townships for a certain
#'          buffer radius.}
#'     \item{This function takes advantage of the Google Maps Geocoding API, and
#'           is limited by the standard usage limit of 2,500 free requests per
#'           day and 50 requests per second.
#' \url{https://developers.google.com/maps/documentation/geocoding/usage-limits}}
#' }
#'
#' @examples
#' \dontrun{
#' clean_pur <- pull_clean_pur(2000, counties = "10")
#' exposure_list <- calculate_exposure(clean_pur,
#'                                     location = "13883 Lassen Ave, Helm, CA 93627",
#'                                     radius = 3000)
#' names(exposure_list)
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
#'                                 location = "-116.45, 34.96",
#'                                 radius = 5000)
#' df_plot(exp_list3$buffer_plot)
#' exp_list3$county_plot
#'
#' # calculate exposure by specified chemical classes
#' # this is an example of `none_recorded = TRUE`
#' chemical_class_df <- rbind(find_chemical_codes(2000, "methylene"),
#'                            find_chemical_codes(2000, "aldehyde")) %>%
#'    dplyr::rename(chemical_class = chemical)
#' exp_list4 <- pull_clean_pur(1995, "fresno",
#'                             chemicals = chemical_class_df$chemname,
#'                             sum_application = TRUE,
#'                             sum = "chemical_class",
#'                             chemical_class = chemical_class_df) %>%
#'    calculate_exposure(location = "13883 Lassen Ave, Helm, CA 93627",
#'                       radius = 1500,
#'                       chemicals = "chemical_class")
#' exp_list4$meta_data
#'
#' # incorporate aerial/ground application information
#' exp_list5 <- pull_clean_pur(2000, "yolo") %>%
#'    calculate_exposure(location = "-121.9018, 38.7646",
#'    radius = 2500,
#'    aerial_ground = TRUE)
#' exp_list5$exposure
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @export
calculate_exposure <- function(clean_pur_df, location, radius,
                               time_period = NULL, start_date = NULL,
                               end_date = NULL, chemicals = "all",
                               aerial_ground = FALSE, verbose = TRUE, ...) { # what is the ... for. @original_location !

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
    latlon_df <- help_geocode(address)
    address_x <- latlon_df$lon
    address_y <- latlon_df$lat
    latlon_out <- as.numeric(c(latlon_df$lon, latlon_df$lat))
  }

  # pull county shapefile
  county <- find_location_county(location, return = "name", latlon_out)$county
  check <- toupper(county) %in% clean_pur_df$county_name
  if (!check) {
    stop(paste0("\"", location,  "\"", " is located in ", county, " county. ",
                "\nThe clean_pur_df data frame doesn't include data for this ",
                "county."))
  }

  clean_pur_df <- clean_pur_df %>% dplyr::filter(county_name == toupper(county))

  if (verbose) {

    # relevant for write_exposure()
    args <- list(...)
    if (!is.null(args$original_location)) {
      loc_message <- args$original_location
    } else {
      loc_message <- location
    }

    if (is.null(time_period) & is.null(start_date) & is.null(end_date)) {
      date_message <- " over the entire date range of the supplied PUR data set"
    } else if (!is.null(time_period)) {
      time_period_message <- stringr::str_replace(time_period, "s", "")
      date_message <- paste0(" over ", time_period_message, " increments")
    } else if (!is.null(start_date) & !is.null(end_date)) {
      date_message <- paste0(" from ", start_date, " to ", end_date)
    }

    if (chemicals == "all") {
      chem_message <- "for all active ingredients in the supplied PUR data set"
    } else if (chemicals == "chemical_class") {
      chem_message <- "for all chemical classes in the supplied PUR data set"
    }

    message(paste0("Calculating exposure for ", radius, " meters extending from ",
                   "the location ", "\"", loc_message, "\"",
                   date_message, " ", chem_message, "."))
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
    shp <- pull_spdf(as.character(county), "section")
    df <- spdf_to_df(shp)
  } else {
    shp <- pull_spdf(as.character(county), "township")
    df <- spdf_to_df(shp)
  }

  context_plot <- ggplot2::ggplot(data = df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group),
                          color = "grey", fill = NA) +
    ggplot2::geom_polygon(data = buffer_df, ggplot2::aes(x = long, y = lat),
                          color = "red", fill = NA) +
    ggplot2::theme_void() +
    ggplot2::coord_map()

  # find sections (and townships) w/in buffer
  which_pls <- df %>% dplyr::filter(long >= range$min_long &
                                       long <= range$max_long &
                                       lat >= range$min_lat &
                                       lat <= range$max_lat)

  if (nrow(which_pls) == 0) {

    if ("section" %in% colnames(clean_pur_df)) {

      borders <- df %>% group_by(MTRS) %>%
        dplyr::summarise(min_long = min(long), min_lat = min(lat),
                         max_long = max(long), max_lat = max(lat))

      corner <- purrr::map2_dfr(borders$max_long, borders$max_lat, help_calc_distance,
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

      corner <- purrr::map2_dfr(borders$max_long, borders$max_lat, help_calc_distance,
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
      year <- lubridate::year(min(clean_pur_df$date))
      start_date <- lubridate::ymd(paste0(year, "-01-01"))
    } else {
      start_date <- lubridate::ymd(start_date)
    }
    if (is.null(end_date)) {
      year <- lubridate::year(max(clean_pur_df$date))
      end_date <- lubridate::ymd(paste0(year, "12-31"))
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

  clean_pur_df <- clean_pur_df %>% dplyr::filter(date >= lubridate::ymd(min(time_df$start_date)) &
                                                   date <= lubridate::ymd(max(time_df$end_date)))

  if ("section" %in% colnames(clean_pur_df)) {
    out_list <- help_filter_pls(MTRS, "MTRS", which_pls, shp, buffer, df, clean_pur_df)
  } else {
    out_list <- help_filter_pls(MTR, "MTR", which_pls, shp, buffer, df, clean_pur_df)
  }

  pur_filt <- out_list$pur_filt
  comb_df_filt <- out_list$comb_df_filt
  pls_percents <- out_list$pls_intersections
  pls_int <- out_list$pls_int

  out <- purrr::map2(time_df$start_date,
                     time_df$end_date,
                     help_calculate_exposure, aerial_ground, chemicals,
                     clean_pur_df, location, pls_percents, pur_filt,
                     radius)

  for (i in 1:length(out)) {
    exp_row <- out[[i]]$row_out[[1]]
    meta_data <- out[[i]]$meta_data[[1]]
    if (i == 1) {
      row_out <- exp_row
      meta_out <- meta_data
    } else {
      row_out <- rbind(row_out, exp_row)
      meta_out <- rbind(meta_out, meta_data)
    }
  }

  row_out <- dplyr::mutate(row_out,
                           longitude = latlon_out[1],
                           latitude = latlon_out[2])

  if (!is.null(args$original_location)) {
    row_out$location <- args$original_location
    meta_data$location <- args$original_location
  }

  out <- list(exposure = row_out,
              meta_data = meta_out,
              buffer_plot_df = comb_df_filt,
              county_plot = context_plot,
              clean_pur_df = clean_pur_df)

  return(out)

}
