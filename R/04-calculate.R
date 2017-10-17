#' Calculate exposure to active ingredients present in applied pesticides.
#'
#' For a particular location, buffer radius, date range, and active ingredient
#' or class of active ingredients, \code{calculate_exposure} calculates an
#' estimate of exposure in kg of active ingredient per m^2.
#'
#' @param location A length-one character string. Either a California address
#'   including street name, city, state, and zip code, or a pair of coordinates
#'   in the form "longitude, latitude".
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
#' @param aerial_ground TRUE / FALSE for whether you would like to
#'   incorporate aerial/ground application into exposure calculations. If
#'   \code{aerial_ground = TRUE}, there should be an \code{aerial_ground}
#'   column in the input \code{clean_pur_df} data frame. There will be a value of
#'   exposure calculated for each chemical ("all" or by chemical class) and for
#'   each method of application: aerial or ground. The default is FALSE.
#'
#' @return A list with four elements:
#'  \describe{
#'    \item{exposure}{A data frame with 7 columns: \code{exposure},
#'    the estimate of exposure in kg/m^2, \code{chemicals}, (either "all",
#'    indicating that all active ingredients present in the \code{clean_pur_df}
#'    were summed or the chemical class(es) specified in the \code{clean_pur_df}
#'    data frame), \code{start_date}, \code{end_date}, \code{aerial_ground},
#'    which can take values of "A" = aerial, "G" = ground, and "O" = others, (if
#'    the \code{aerial_ground} argument is \code{FALSE}, \code{aerial_ground}
#'    will be \code{NA} in the \code{exposure} data frame), \code{location}, and
#'    \code{radius}, the radius in meters for the buffer extending from the
#'    location}
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
#'    or township, date range, and chemicals, \code{location}, and \code{radius}}
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
#'          list element to take a look at the \code{county_plot} plot element
#'          could be helpful to see the difference between calculating exposure
#'          based on sections or townships for a certain buffer radius.}
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
#' plot_tidy(exp_list3$buffer_plot)
#' exp_list3$county_plot
#'
#' # calculate exposure by specified chemical classes
#' # this is an example of `none_recorded = TRUE`
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
#' exp_list4$meta_data
#'
#' # incorporate aerial/ground application information
#' clean_pur4 <- pull_clean_pur(2000, "yolo", aerial_ground = TRUE)
#' exp_list5 <- calculate_exposure(clean_pur4,
#'                                 location = "-121.9018, 38.7646",
#'                                 radius = 2500,
#'                                 aerial_ground = TRUE)
#' exp_list5$exposure
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom dplyr !!
#' @importFrom dplyr !!!
#' @export
calculate_exposure <- function(clean_pur_df, location, radius,
                               time_period = NULL, start_date = NULL,
                               end_date = NULL, chemicals = "all",
                               aerial_ground = FALSE) {

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
  county <- find_location_county(location, return = "name", latlon_out)
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

  # if ("section" %in% colnames(clean_pur_df)) {
  #   shp <- pull_spdf(county, "section")
  #   df <- spdf_to_df(shp)
  # } else {
  #   shp <- pull_spdf(county, "township")
  #   df <- spdf_to_df(shp)
  # }

  ### ftp is down (replaces ^)

  if ("section" %in% colnames(clean_pur_df)) {
    shp <- readRDS("~/Documents/pesticides_project/data/fresno_shp.rds")
    df <- readRDS("~/Documents/pesticides_project/data/fresno_coords.rds")
  } else {
    shp <- readRDS("~/Documents/pesticides_project/data/fresno_township_shp.rds")
    df <- readRDS("~/Documents/pesticides_project/data/fresno_township_coords.rds")
  }

  ###

  context_plot <- ggplot2::ggplot(data = df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group),
                          color = "grey", fill = NA) +
    ggplot2::geom_polygon(data = buffer_df, ggplot2::aes(x = long, y = lat),
                          color = "red", fill = NA) +
    ggplot2::theme_void()

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

  # check_dates
  check_min_date <- min(time_df$start_date) == min(clean_pur_df$date)
  check_max_date <- max(time_df$end_date) == max(clean_pur_df$date)

  if (!check_min_date | !check_max_date) {
    stop("Your pur_clean_df data frame doesn't contain data for the entire date\n ",
         "range speficied by start_date and end_date. You may need to pull data\n ",
         "for additional years using the pull_clean_pur() function." )
  }

 ##

  pur_filt_df <- function(pls, pls_quote) {

    pls_var <- rlang::enquo(pls)
    pls_name <- rlang::quo_name(pls_quote)

    tibble_to_vector <- function(tib) {
      vec <- tib %>% dplyr::pull(1) %>% as.character()
      return(vec)
    }

    pls_int <- which_pls %>%
      tibble::as_tibble() %>%
      dplyr::select(!!pls_name) %>%
      unique() %>%
      tibble_to_vector()

    # filter shp file to include only sections intersecting w/ buffer
    df_filtered <- df %>% dplyr::filter(rlang::UQ(pls_var) %in% pls_int)

    # combine buffer to existing sections shp file
    buffer_shp <- shp[1, ]
    buffer_shp@polygons[[1]]@Polygons[[1]]@coords <- buffer
    buffer_shp <- sp::spChFIDs(buffer_shp, paste("buffer", 1:nrow(buffer_shp), sep = ""))

    if (pls_quote == "MTRS") {
      buffer_shp@data$MTRS <- "buffer"
    } else {
      buffer_shp@data$MTR <- "buffer"
    }

    comb_shp <- maptools::spRbind(shp, buffer_shp)
    suppressMessages(comb_df <- broom::tidy(comb_shp))

    if (pls_quote == "MTRS") {
      comb_shp_filt <- subset(comb_shp, MTRS %in% c(pls_int, "buffer"))
    } else {
      comb_shp_filt <- subset(comb_shp, MTR %in% c(pls_int, "buffer"))
    }

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

      if (pls_quote == "MTRS") {
        percent_df <- data.frame(MTRS = comb_shp_filt@data$MTRS[i],
                                 percent = intersected_area/section_area)
      } else {
        percent_df <- data.frame(MTR = comb_shp_filt@data$MTR[i],
                                 percent = intersected_area/section_area)
      }

      if (i == 1) {
        out <- percent_df
      } else {
        out <- rbind(out, percent_df)
      }
    }

    out <- dplyr::filter(out, percent != 0)
    if (pls_quote == "MTRS") {
      comb_shp_filt <- subset(comb_shp_filt, MTRS %in% c(as.character(out$MTRS),
                                                         "buffer"))
    } else {
      comb_shp_filt <- subset(comb_shp_filt, MTR %in% c(as.character(out$MTR),
                                                        "buffer"))
    }

    suppressMessages(comb_df_filt <- broom::tidy(comb_shp_filt))

    comb_df_filt <- dplyr::mutate(comb_df_filt, group = as.character(group))

    comb_df_filt <- df_filtered %>%
      dplyr::mutate(group = as.character(group)) %>%
      dplyr::right_join(comb_df_filt, by = c("long", "lat", "hole", "order",
                                             "piece", "id", "group"))

    pur_filt <- dplyr::filter(clean_pur_df, !is.na(kg_chm_used))
    out <- list(pur_filt = pur_filt,
                comb_df_filt = comb_df_filt,
                pls_intersections = out,
                pls_int = pls_int)
    return(out)

  }

  if ("section" %in% colnames(clean_pur_df)) {
    out_list <- pur_filt_df(MTRS, "MTRS")
  } else {
    out_list <- pur_filt_df(MTR, "MTR")
  }

  pur_filt <- out_list$pur_filt
  comb_df_filt <- out_list$comb_df_filt
  pls_percents <- out_list$pls_intersections
  pls_int <- out_list$pls_int

  ## calculate exposure for multiple start and end dates
  ## returns a nested data frame - \code{row_out} and \code{meta_data}
  calculate_exposure_bydate <- function(start_date, end_date) {

    pur_out_df <- function(...) {
      group_by <- rlang::quos(...)
      pur_out <- pur_filt %>%
        dplyr::filter(date >= start_date & date <= end_date) %>%
        dplyr::group_by(!!!group_by) %>%
        summarise(kg = sum(kg_chm_used)) %>%
        dplyr::ungroup()
      return(pur_out)
    }

    if (chemicals == "all") {
      if ("section" %in% colnames(pur_filt)) {
        if (aerial_ground) {
          pur_out <- pur_out_df(section, aerial_ground)
        } else {
          pur_out <- pur_out_df(section)
        }
      } else {
        if (aerial_ground) {
          pur_out <- pur_out_df(township, aerial_ground)
        } else {
          pur_out <- pur_out_df(township)
        }
      }
    } else {
      if ("section" %in% colnames(pur_filt)) {
        if (aerial_ground) {
          pur_out <- pur_out_df(section, chemical_class, aerial_ground)
        } else {
          pur_out <- pur_out_df(section, chemical_class)
        }
      } else {
        if (aerial_ground) {
          pur_out <- pur_out_df(township, chemical_class, aerial_ground)
        } else {
          pur_out <- pur_out_df(township, chemical_class)
        }
      }
    }

    buffer_area <- pi * (radius^2)

    exp_df <- function(mtrs_mtr, section_township) {

      mutate_expr <- rlang::enquo(mtrs_mtr)
      rename_expr <- rlang::enquo(section_township)

      if ("chemical_class" %in% colnames(clean_pur_df)) {

        classes <- unique(clean_pur_df$chemical_class)
        n_classes <- length(classes)
        pls <- as.character(pls_percents[,1])
        n_pls <- length(pls)

        class_pls <- data.frame(pls = as.character(rep(pls, times = n_classes)),
                                chemicals = rep(classes, each = n_pls)) %>%
          dplyr::mutate(pls = as.character(pls))

        pur_out2 <- pur_out %>%
          dplyr::rename(pls := !!rename_expr,
                        chemicals = chemical_class) %>%
          dplyr::mutate(chemicals = as.character(chemicals))

        exp_0 <- pls_percents %>%
          dplyr::rename(pls := !!mutate_expr) %>%
          dplyr::mutate(pls = as.character(pls)) %>%
          dplyr::full_join(class_pls, by = "pls") %>%
          dplyr::arrange(chemicals) %>%
          dplyr::mutate(chemicals = as.character(chemicals)) %>%
          dplyr::left_join(pur_out2, by = c("pls", "chemicals")) %>% # brings in kg, maybe aerial_ground
          dplyr::mutate(location = location,
                        kg = ifelse(is.na(kg), 0, kg),
                        none_recorded = ifelse(kg == 0, TRUE, FALSE),
                        kg_intersection = percent * kg,
                        start_date = start_date,
                        end_date = end_date,
                        radius = radius,
                        area = buffer_area)
      } else {

        class_pls <- data.frame(pls = as.character(pls_percents[,1]),
                                chemicals = "all") %>%
          dplyr::mutate(pls = as.character(pls))

        pur_out2 <- pur_out %>%
          dplyr::rename(pls := !!rename_expr)

        exp_0 <- pls_percents %>%
          dplyr::rename(pls := !!mutate_expr) %>%
          dplyr::mutate(pls = as.character(pls)) %>%
          dplyr::full_join(class_pls, by = "pls") %>%
          dplyr::mutate(chemicals = as.character(chemicals)) %>%
          dplyr::left_join(pur_out2, by = "pls") %>% # brings in kg, maybe aerial_ground
          dplyr::mutate(location = location,
                        kg = ifelse(is.na(kg), 0, kg),
                        none_recorded = ifelse(kg == 0, TRUE, FALSE),
                        kg_intersection = percent * kg,
                        start_date = start_date,
                        end_date = end_date,
                        radius = radius,
                        area = buffer_area)

      }

      if ("aerial_ground" %in% colnames(exp_0)) {
        exp <- exp_0 %>% dplyr::mutate(aerial_ground = as.character(aerial_ground))
      } else {
        exp <- exp_0 %>% dplyr::mutate(aerial_ground = NA)
      }

      exp <- exp %>%
        dplyr::select(pls, chemicals, percent, kg, kg_intersection, start_date,
                      end_date, aerial_ground, none_recorded, location, radius, area)

      return(exp)

    }

    if ("section" %in% colnames(pur_filt)) {
      exp <- exp_df(MTRS, section)
    } else {
      exp <- exp_df(MTR, township)
    }

    exp_out_df <- function(...) { # to calculate exposure, either want to
      # group by only chemicals or by chemicals
      # and aerial_ground
      group_by_vars <- rlang::quos(...)
      exp_out <- exp %>%
        dplyr::group_by(!!!group_by_vars) %>%
        dplyr::summarise(exposure = sum(kg_intersection, na.rm = TRUE) / buffer_area)
      return(exp_out)
    }

    row_out_df <- function(...) { # either chemicals or chemicals, aerial_ground
      vars <- rlang::quos(...)
      row_out_0 <- exp_out_df(!!!vars)
      if ("aerial_ground" %in% colnames(row_out_0)) {
        row_out <- row_out_0 %>%
          dplyr::mutate(start_date = start_date,
                        end_date = end_date,
                        location = location,
                        radius = radius) %>%
          dplyr::select(exposure, chemicals, start_date, end_date, aerial_ground,
                        location, radius)

      } else {
        row_out <- row_out_0 %>%
          dplyr::mutate(start_date = start_date,
                        end_date = end_date,
                        aerial_ground = NA,
                        location = location,
                        radius = radius) %>%
          dplyr::select(exposure, chemicals, start_date, end_date, aerial_ground,
                        location, radius)
      }

      return(row_out)

    }

    if (aerial_ground) {
      row_out <- row_out_df(chemicals, aerial_ground)
    } else {
      row_out <- row_out_df(chemicals)
    }

    nested_df <- list(row_out = list(row_out), meta_data = list(exp))
    attr(nested_df, "row.names") <- as.integer(1)
    class(nested_df) <- c("tbl_df", "data.frame")

    return(nested_df)

  }

  out <- purrr::map2(time_df$start_date, time_df$end_date,
                     calculate_exposure_bydate)
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

  meta_out <- unique(meta_out)

  out <- list(exposure = row_out,
              meta_data = meta_out,
              buffer_plot_df = comb_df_filt,
              county_plot = context_plot)

  return(out)

}


