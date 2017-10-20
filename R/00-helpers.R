#' Read in PUR county dataset for a year.
#'
#' Once a year of PUR data has been downloaded, this function reads in a
#' selected county's dataset.
#'
#' This is a helper function for \code{pull_pur_file}.
#'
#' @param code_or_file A PUR county code or a file name for a county's dataset.
#' @param type Either "codes" or "files", specifying the type of argument supplied
#' to \code{code_or_file}.
#' @param year A four digit year.
#'
#' @return A data frame of raw PUR data for a single county and year.
read_in_counties <- function(code_or_file, type, year) {

  sm_year <- substr(year, 3, 4)

  if (type == "codes") {

    raw_data <- suppressWarnings(suppressMessages(
      readr::read_csv(paste0("udc", sm_year, "_", code_or_file, ".txt"))))
    raw_data <- dplyr::mutate_all(raw_data, as.character)

    return(raw_data)

  } else if (type == "files") {

    raw_data <- suppressWarnings(suppressMessages(
      readr::read_csv(code_or_file)))
    raw_data <- dplyr::mutate_all(raw_data, as.character)

    return(raw_data)

  }
}

#' Find California county code or name
#'
#' Given a county, \code{single_county_code} returns either the PUR
#' county code or name.
#'
#' This is a helper function for \code{find_counties}.
#'
#' @param county A character string giving either a county name or
#'  two digit PUR county code. Not case sensitive. California names and county
#'  codes as they appear in PUR datasets can be found in the \code{county_codes}
#'  dataset available with this package.
#' @param return Either "codes" to return county code (the default) or "names"
#'  to return county name.
#'
#' @return If \code{return = "codes"}, a two-character string giving
#'   the corresponding PUR county codes. If \code{return = "names"}, a county
#'   name.
#'
#' @examples
#' single_county_code("01", find = "names")
#' single_county_code("contra costa", find = "codes")
#' @importFrom magrittr %>%
single_county_code <- function(county, find = "codes") {

  code_df <- purexposure::county_codes

  test <- suppressWarnings(as.numeric(county))

  if (is.na(test)) {

    county_upper <- toupper(county)
    county_nm <- grep(county_upper, code_df$county_name, value = TRUE)

    if (length(county_nm) != 1) {
      error <- "yes"
    } else {
      error <- NULL

      code <- as.character(code_df %>%
                             dplyr::filter(county_name == county_nm) %>%
                             dplyr::select(county_code))

      name <- strsplit(tolower(county_nm), " ")[[1]]
      name <- paste(toupper(substring(name, 1,1)), substring(name, 2),
                    sep = "", collapse = " ")
    }

  } else {

    county_cd <- county
    code <- grep(county_cd, code_df$county_code, value = TRUE)

    if (length(code) != 1) {
      error <- "yes"
    } else {
      error <- NULL

      county_nm <- as.character(code_df %>%
                                  dplyr::filter(county_code == code) %>%
                                  dplyr::select(county_name))

      name <- strsplit(tolower(county_nm), " ")[[1]]
      name <- paste(toupper(substring(name, 1,1)), substring(name, 2),
                    sep = "", collapse = " ")
    }
  }

  if (is.null(error)) {
    if (find == "codes") {
      return(code)
    } else if (find == "names") {
      return(name)
    }
  } else {
    return(NULL)
  }

}

#' Plot data frame spatial objects.
#'
#' \code{df_plot} plots a data frame spatial object. (A
#' SpatialPolygonsDataFrame that has been "tidied" using the broom package.)
#' Meant to be analogous to the ease of using plot() to quickly view a
#' SpatialPolygonDataFrame object.
#'
#' @param df A data frame returned from the \code{spdf_to_df} function.
#'
#' @return A ggplot2 plot of the county.
#'
#' @examples
#' \dontrun{
#' shp <- pull_spdf("san diego", "township")
#' df <- spdf_to_df(shp)
#' df_plot(df)
#' }
#' @export
df_plot <- function(df) {

  plot <- ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat,
                                                  group = group)) +
    ggplot2::geom_polygon(color = "black", fill = NA) +
    ggplot2::theme_void()

  return(plot)

}

#' Convert county SpatialPolygonsDataFrame to a tidy data frame.
#'
#' \code{spdf_to_df} converts a SpatialPolygonsDataFrame object returned from
#' the \code{pull_spdf} function to a data frame.
#'
#' @param spdf A SpatialPolygonsDataFrame object returned from
#' the \code{pull_spdf} function.
#'
#' @return A data frame with 24 columns if the \code{spdf} object is on the
#' section level and 23 columns if the \code{spdf} object is on the township
#' level.
#'
#' @examples
#' \dontrun{
#' df <- spdf_to_df(pull_spdf("fresno"))
#' df2 <- spdf_to_df(pull_spdf("sonoma"))
#'
#' # use df_plot() function to easily plot the output data frames:
#' df_plot(df)
#' df_plot(df2)
#' }
#' @importFrom magrittr %>%
#' @export
spdf_to_df <- function(spdf) {

  df <- suppressMessages(sp::merge(broom::tidy(spdf), as.data.frame(spdf),
                                   by.x = "id", by.y = 0))

  if ("MTRS" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(MTRS = as.character(MTRS))
  }
  if ("MTR" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(MTR = as.character(MTR))
  }

  return(df)

}

#' Sum application
#'
#' \code{sum_application_by} sums application of a PUR dataset by chemicals,
#' PLS unit, and aerial/ground application.
#'
#' This is a helper function for \code{pull_clean_pur}.
#'
#' @param df A data frame from \code{pull_clean_pur} before summing has taken
#' place
#' @param sum "all" or "chemical_class"
#' @param unit either "section" or "township"
#' @param aerial_ground TRUE / FALSE
#' @param ... grouping variables
#'
#' @return A data frame. The number of columns is dependent on the grouping
#' variables supplied to the \code{...} argument.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
sum_application_by <- function(df, sum, unit, aerial_ground, ...) {

  group_by_vars <- rlang::quos(...)

  if (sum == "chemical_class") {
    df <- df %>%
      dplyr::left_join(chemical_class, by = c("chem_code", "chemname")) %>%
      dplyr::mutate(chemical_class = ifelse(is.na(chemical_class), "other",
                                            chemical_class))
  }

  df <- df %>%
    dplyr::group_by(!!!group_by_vars) %>%
    dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if (unit == "section") {
    df <- df %>% dplyr::left_join(section_townships, by = "section")
  }

  all_cols <- c("chem_code", "chemname", "chemical_class", "kg_chm_used",
                "section", "township", "county_name", "county_code",
                "date", "aerial_ground", "use_no", "outlier")

  missing <- all_cols[!all_cols %in% colnames(df)]

  for (i in 1:length(missing)) {
    new_col <- rlang::quo_name(missing[i])
    df <- df %>% dplyr::mutate(!!new_col := NA)
  }

  df <- df %>%
    dplyr::arrange(date, county_name, county_code, chemical_class, chemname,
                   chem_code, section, township) %>%
    dplyr::select(chem_code, chemname, chemical_class, kg_chm_used,
                  section, township, county_name, county_code,
                  date, aerial_ground, use_no, outlier)

  cols_to_remove <- purrr::map_dfr(colnames(df), remove_cols, df = df) %>%
    dplyr::filter(all_missing == T) %>%
    dplyr::select(col) %>%
    tibble_to_vector()

  cols_to_keep <- colnames(df)[!colnames(df) %in% cols_to_remove]
  cols_names <- unlist(purrr::map(cols_to_keep, quo_name))
  df <- df %>% dplyr::select(!!cols_names)

  return(df)

}

#' Remove columns with all missing values
#'
#' Given a quoted column name and its data frame, \code{remove_cols} determines
#' if that column has all missing values or not.
#'
#' This is a helper function for \code{sum_application}.
#'
#' @param col_quote A quoted column name
#' @param df A data frame
#'
#' @return A data frame with two columns: \code{col} gives the column name, and
#' \code{all_missing} is a logical value.
remove_cols <- function(col_quote, df) {

  col_name <- rlang::quo_name(col_quote)
  vals <- df %>% dplyr::select(!!col_name) %>% tibble_to_vector()
  logical_val <- all(is.na(vals))
  out_df <- data.frame(col = col_name, all_missing = logical_val)

  return(out_df)

}

#' Calculate euclidean distance between two points.
#'
#' \code{euc_distance} calculates the straight-line distance between
#' two points.
#'
#' This is a helper function for \code{calculate_exposure}.
#'
#' @param long Longitude (x) of second point
#' @param lat Latitude (y) of second point
#' @param origin_long Longitude (x) of first point
#' @param origin_lat Latitude (y) of first point
#'
#' @return A data frame with one row and three columns: \code{long} and
#' \code{lat} give the second point's coordinates, and \code{dist} gives the
#' euclidian distance from these coordinates from the origin.
euc_distance <- function(long, lat, origin_long, origin_lat) {

  x <- abs(lat - origin_lat)
  y <- abs(long - origin_long)
  dist <- sqrt((x^2) + (y^2))
  out <- data.frame(long = long, lat = lat, dist = dist)

  return(out)

}

#' Return a character vector from a tibble with one column.
#'
#' \code{tibble_to_vector} takes a tibble with one column and returns the
#' values in that column as a character vector.
#'
#' This is a helper function for \code{pull_raw_pur}, \code{pull_clean_pur},
#' and \code{pur_filt_df}.
#'
#' @param tib A tibble with only one column.
#'
#' @return A character vector.
#' @importFrom magrittr %>%
tibble_to_vector <- function(tib) {

  vec <- tib %>% dplyr::pull(1) %>% as.character()

  return(vec)

}

#' Find PLS units intersecting with a buffer.
#'
#' \code{pur_filt_df} filters a SpatialPolygonsDataFrame to include only PLS
#' units intersecting with a buffer, and filters the data frame returned from
#' \code{pull_clean_pur} to include only those sections or townships.
#'
#' This is a helper function for \code{calculate_exposure}.
#'
#' @param pls Either \code{MTRS} (sections) or \code{MTR} (townships). Not quoted.
#' @param pls_quote Either \code{"MTRS"} or \code{"MTR"}
#' @param which_pls A vector of character string PLS units
#' @param shp A county's shapefile.
#' @param buffer A data frame with buffer coordinates.
#' @param df
#' @param clean_pur_df
#'
#' @return A list with four elements:
#' \describe{
#'   \item{pur_filt}{A cleaned PUR data frame filtered to PLS units intersecting
#'   with a buffer.}
#'   \item{comb_df_filt}{A spatial data frame with intersecting PLS units and
#'   a buffer.}
#'   \item{pls_intersections}{A data frame with two columns: \code{pls} and
#'   \code{percent}, the corresponding percent intersection with the buffer.}
#'   \item{pls_int}{A character vector with all PLS units intersecting with the
#'   buffer.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang !!
pur_filt_df <- function(pls, pls_quote, which_pls, shp, buffer, df,
                        clean_pur_df) {

  pls_var <- rlang::enquo(pls)
  pls_name <- rlang::quo_name(pls_quote)

  pls_int <- which_pls %>%
    tibble::as_tibble() %>%
    dplyr::select(!!pls_name) %>%
    unique() %>%
    tibble_to_vector()

  # filter shp file to include only sections intersecting w/ buffer-
  # df_filtered <- df %>% dplyr::filter(rlang::UQ(pls_var) %in% pls_int)

  if (pls_name == "MTRS") {
    df_filtered <- df %>% dplyr::filter(MTRS %in% pls_int)
  } else if (pls_name == "MTR") {
    df_filtered <- df %>% dplyr::filter(MTR %in% pls_int)
  }

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

#' Find summed applied active ingredients
#'
#' \code{pur_out_df} finds the summed amount of applied active ingredients by
#' section or township, chemical class, and aerial/ground application.
#'
#' This is a helper function for \code{daterange_calcexp}.
#'
#' @param ... A list of variables to group by. Options include \code{section},
#' \code{township}, \code{chemical_class}, and \code{aerial_ground}. Not quoted.
#' @param pur_filt
#' @param start_date
#' @param end_date
#'
#' @return A data frame a \code{kg} column and one to three additional columns,
#' depending on the grouping variables.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
pur_out_df <- function(pur_filt, start_date, end_date, ...) {

  group_by <- rlang::quos(...)

  pur_summed <- pur_filt %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::group_by(!!!group_by) %>%
    dplyr::summarise(kg = sum(kg_chm_used)) %>%
    dplyr::ungroup()

  return(pur_summed)

}

#' Return a the \code{meta_data} data frame
#'
#' \code{exp_df} returns a data frame to be output as the \code{meta_data}
#' element in the list returned from \code{calculate_exposure}.
#'
#' This is a helper function for \code{daterange_calcexp}.
#'
#' @param mtrs_mtr Either \code{MTRS} or \code{MTR}. Not quoted.
#' @param section_township Either \code{section} or \code{township}. Not quoted.
#' @param clean_pur_df,
#' @param pls_percents
#' @param pls
#' @param pur_out
#' @param location
#' @param start_date
#' @param end_date
#' @param radius
#' @param buffer_area
#'
#' @return A data frame with the twelve columns in the
#' \code{calculate_exposure$meta_data} data frame.
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom rlang :=
exp_df <- function(clean_pur_df, pls_percents, pls, pur_out, location,
                   start_date, end_date, radius, buffer_area,
                   mtrs_mtr, section_township) {

  mutate_expr <- rlang::enquo(mtrs_mtr)
  rename_expr <- rlang::enquo(section_township)

  if ("chemical_class" %in% colnames(clean_pur_df)){

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

  if ("aerial_ground" %in% colnames(exp_0)){
    exp <- exp_0 %>% dplyr::mutate(aerial_ground = as.character(aerial_ground))
  } else {
    exp <- exp_0 %>% dplyr::mutate(aerial_ground = NA)
  }

  exp <- exp %>%
    dplyr::select(pls, chemicals, percent, kg, kg_intersection, start_date,
                  end_date, aerial_ground, none_recorded, location, radius, area)

  return(exp)

}

#' Return a single exposure value for each combination of conditions.
#'
#' \code{exp_out_val} returns a data frame with exposure values (kg/m^2) by
#' chemicals (including \code{chemicals = "all"}) or by chemicals and aerial/ground
#' application.
#'
#' This is a helper function for \code{daterange_calcexp}.
#'
#' @param ... Either \code{chemicals} or \code{chemicals, aerial_ground}. Not
#' quoted.
#' @param exp exp data frame
#' @param buffer_area numeric value
#'
#' @return A data frame with exposure values in kg/m^2 at a location for each
#' relevant condition.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
exp_out_val <- function(exp, buffer_area, ...) {

  group_by_vars <- rlang::quos(...)

  exp_out <- exp %>%
    dplyr::group_by(!!!group_by_vars) %>%
    dplyr::summarise(exposure = sum(kg_intersection, na.rm = TRUE) / buffer_area)

  return(exp_out)

}

#' Return a data frame with exposure values and other related data.
#'
#' For a single date range, \code{row_out_df} returns a data frame with exposure
#' values calculated from \code{exp_out_val} as well as other relevant data.
#' This one row data frame is combined with data frames for other date ranges and
#' then returned as the \code{exposure} element from a \code{calculate_exposure}
#' list.
#'
#' @param ... Either \code{chemicals} or \code{chemicals, aerial_ground}. Not
#'   quoted.
#' @param start_date
#' @param end_date
#' @param location
#' @param radius
#'
#' @return A data frame with one row and the columns found in the
#' \code{calculate_exposure$exposure} data frame.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
row_out_df <- function(start_date, end_date, location, radius,
                       exp, buffer_area, ...) {

  vars <- rlang::quos(...)

  row_out_0 <- exp_out_val(exp, buffer_area, !!!vars)

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

#' Return exposure data for a single start and end date
#'
#' For a single date range, \code{daterange_calcexp} returns the
#' \code{exposure} and \code{meta_data} data frames to be output in the
#' \code{calculate_exposure} list as a nested data frame.
#'
#' This is a helper function for \code{calculate_exposure}.
#'
#' @param start_date A date, "yyyy-mm-dd"
#' @param end_date A date, "yyyy-mm-dd"
#' @inheritParams exp_df
#' @inheritParams pur_out_df
#' @inheritParams row_out_df
#' @inheritParams exp_out_val
#' @param chemicals
#' @param aerial_ground
#'
#' @return A nested data frame with two columns: The \code{row_out} column
#' contains the \code{exposure} data frame for the date range, and
#' \code{meta_data} contains the \code{meta_data} data frame for the date range.
daterange_calcexp <- function(start_date, end_date, aerial_ground,
                              buffer_area, chemicals,
                              clean_pur_df,
                              exp, location, pls, pls_percents,
                              pur_filt, pur_out, radius) {

  if (chemicals == "all") {
    if ("section" %in% colnames(pur_filt)) {
      if (aerial_ground) {
        pur_out <- pur_out_df(pur_filt, start_date, end_date,
                              section, aerial_ground)
      } else {
        pur_out <- pur_out_df(pur_filt, start_date, end_date,
                              section)
      }
    } else {
      if (aerial_ground) {
        pur_out <- pur_out_df(pur_filt, start_date, end_date,
                              township, aerial_ground)
      } else {
        pur_out <- pur_out_df(pur_filt, start_date, end_date, township)
      }
    }
  } else {
    if ("section" %in% colnames(pur_filt)) {
      if (aerial_ground) {
        pur_out <- pur_out_df(pur_filt, start_date, end_date,
                              section, chemical_class, aerial_ground)
      } else {
        pur_out <- pur_out_df(pur_filt, start_date, end_date,
                              section, chemical_class)
      }
    } else {
      if (aerial_ground) {
        pur_out <- pur_out_df(pur_filt, start_date, end_date,
                              township, chemical_class, aerial_ground)
      } else {
        pur_out <- pur_out_df(pur_filt, start_date, end_date,
                              township, chemical_class)
      }
    }
  }

  buffer_area <- pi * (radius^2)

  if ("section" %in% colnames(pur_filt)) {
    exp <- exp_df(clean_pur_df, pls_percents, pls, pur_out, location, start_date,
                  end_date, radius, buffer_area,
                  MTRS, section)
  } else {
    exp <- exp_df(clean_pur_df, pls_percents, pls, pur_out, location, start_date,
                  end_date, radius, buffer_area,
                  MTR, township)
  }

  if (aerial_ground) {
    row_out <- row_out_df(start_date, end_date, location, radius, exp, buffer_area,
                          chemicals, aerial_ground)
  } else {
    row_out <- row_out_df(start_date, end_date, location, radius, exp, buffer_area,
                          chemicals)
  }

  nested_df <- list(row_out = list(row_out), meta_data = list(exp))
  attr(nested_df, "row.names") <- as.integer(1)
  class(nested_df) <- c("tbl_df", "data.frame")

  return(nested_df)

}
