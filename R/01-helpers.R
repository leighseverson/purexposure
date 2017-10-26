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
help_read_in_counties <- function(code_or_file, type, year) {

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
#' Given a county, \code{help_find_code} returns either the PUR
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
#' help_find_code("01", find = "names")
#' help_find_code("contra costa", find = "codes")
#' @importFrom magrittr %>%
help_find_code <- function(county, find = "codes") {

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

#' Sum application
#'
#' \code{help_sum_application} sums application of a PUR dataset by chemicals,
#' PLS unit, and aerial/ground application.
#'
#' This is a helper function for \code{pull_clean_pur}.
#'
#' @param df A data frame from \code{pull_clean_pur} before summing has taken
#' place
#' @param sum "all" or "chemical_class"
#' @param unit either "section" or "township"
#' @param aerial_ground TRUE / FALSE
#' @param chemical_class A chemical_class data frame
#' @param section_townships A section_townships data frame
#' @param ... grouping variables
#'
#' @return A data frame. The number of columns is dependent on the grouping
#' variables supplied to the \code{...} argument.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
help_sum_application <- function(df, sum, unit, aerial_ground,
                                 section_townships, chemical_class = NULL,
                                 ...) {

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

  cols_to_remove <- purrr::map_dfr(colnames(df), help_remove_cols, df = df) %>%
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
#' Given a quoted column name and its data frame, \code{help_remove_cols} determines
#' if that column has all missing values or not.
#'
#' This is a helper function for \code{sum_application}.
#'
#' @param col_quote A quoted column name
#' @param df A data frame
#'
#' @return A data frame with two columns: \code{col} gives the column name, and
#' \code{all_missing} is a logical value.
help_remove_cols <- function(col_quote, df) {

  col_name <- rlang::quo_name(col_quote)
  vals <- df %>% dplyr::select(!!col_name) %>% tibble_to_vector()
  logical_val <- all(is.na(vals))
  out_df <- data.frame(col = col_name, all_missing = logical_val)

  return(out_df)

}

#' Find PLS units intersecting with a buffer.
#'
#' \code{help_filter_pls} filters a SpatialPolygonsDataFrame to include only PLS
#' units intersecting with a buffer, and filters the data frame returned from
#' \code{pull_clean_pur} to include only those sections or townships.
#'
#' This is a helper function for \code{calculate_exposure}.
#'
#' @param pls Either \code{MTRS} (sections) or \code{MTR} (townships). Not quoted
#' @param pls_quote Either \code{"MTRS"} or \code{"MTR"}
#' @param which_pls A vector of character string PLS units
#' @param shp A county's shapefile
#' @param buffer A data frame with buffer coordinates
#' @param df A data frame
#' @param clean_pur_df A \code{pull_clean_pur} data frame
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
help_filter_pls <- function(pls, pls_quote, which_pls, shp, buffer, df,
                        clean_pur_df) {

  pls_var <- rlang::enquo(pls)
  pls_name <- rlang::quo_name(pls_quote)

  pls_int <- which_pls %>%
    tibble::as_tibble() %>%
    dplyr::select(!!pls_name) %>%
    unique() %>%
    tibble_to_vector()

  # filter shp file to include only sections intersecting w/ buffer-
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

#' Find summed applied active ingredients
#'
#' \code{help_sum_ai} finds the summed amount of applied active ingredients by
#' section or township, chemical class, and aerial/ground application.
#'
#' This is a helper function for \code{help_calculate_exposure}.
#'
#' @param ... A list of variables to group by. Options include \code{section},
#' \code{township}, \code{chemical_class}, and \code{aerial_ground}. Not quoted
#' @param pur_filt A data frame
#' @param start_date A date
#' @param end_date A date
#'
#' @return A data frame a \code{kg} column and one to three additional columns,
#' depending on the grouping variables.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
help_sum_ai <- function(pur_filt, start_date, end_date, ...) {

  group_by <- rlang::quos(...)

  pur_summed <- pur_filt %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::group_by(!!!group_by) %>%
    dplyr::summarise(kg = sum(kg_chm_used)) %>%
    dplyr::ungroup()

  return(pur_summed)

}

#' Return a \code{meta_data} data frame
#'
#' \code{help_write_md} returns a data frame to be output as the \code{meta_data}
#' element in the list returned from \code{calculate_exposure}.
#'
#' This is a helper function for \code{help_calculate_exposure}.
#'
#' @param mtrs_mtr Either \code{MTRS} or \code{MTR}. Not quoted
#' @param section_township Either \code{section} or \code{township}. Not quoted
#' @param clean_pur_df A data frame
#' @param pls_percents A data frame with PLS ids and percent intersections
#' @param pur_out A data frame
#' @param location A string giving the exposure estimate location
#' @param start_date A date
#' @param end_date A date
#' @param radius A numeric value
#' @param buffer_area A numeric value
#'
#' @return A data frame with the twelve columns in the
#' \code{calculate_exposure$meta_data} data frame.
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom rlang :=
help_write_md <- function(clean_pur_df, pls_percents, pur_out, location,
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
#' \code{help_calc_exp} returns a data frame with exposure values (kg/m^2) by
#' chemicals (including \code{chemicals = "all"}) or by chemicals and aerial/ground
#' application.
#'
#' This is a helper function for \code{help_calculate_exposure}.
#'
#' @param ... Either \code{chemicals} or \code{chemicals, aerial_ground}. Not
#' quoted
#' @param exp A data frame
#' @param buffer_area A numeric value
#'
#' @return A data frame with exposure values in kg/m^2 at a location for each
#' relevant condition.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
help_calc_exp <- function(exp, buffer_area, ...) {

  group_by_vars <- rlang::quos(...)

  exp_out <- exp %>%
    dplyr::group_by(!!!group_by_vars) %>%
    dplyr::summarise(exposure = sum(kg_intersection, na.rm = TRUE) / buffer_area)

  return(exp_out)

}

#' Return a data frame with exposure values and other related data.
#'
#' For a single date range, \code{help_return_exposure} returns a data frame with
#' exposure values calculated from \code{help_calc_exp} as well as other relevant
#' data. This one row data frame is combined with data frames for other date
#' ranges and then returned as the \code{exposure} element from a
#' \code{calculate_exposure} list.
#'
#' This is a helper function for \code{help_calculate_exposure}.
#'
#' @param ... Either \code{chemicals} or \code{chemicals, aerial_ground}. Not
#'   quoted.
#' @inheritParams help_write_md
#'
#' @return A data frame with one row and the columns found in the
#' \code{calculate_exposure$exposure} data frame.
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
help_return_exposure <- function(start_date, end_date, location, radius,
                                 exp, buffer_area, ...) {

  vars <- rlang::quos(...)

  row_out_0 <- help_calc_exp(exp, buffer_area, !!!vars)

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
#' For a single date range, \code{help_calculate_exposure} returns the
#' \code{exposure} and \code{meta_data} data frames to be output in the
#' \code{calculate_exposure} list as a nested data frame.
#'
#' This is a helper function for \code{calculate_exposure}.
#'
#' @param start_date A date, "yyyy-mm-dd"
#' @param end_date A date, "yyyy-mm-dd"
#' @inheritParams help_write_md A data frame
#' @inheritParams pur_out_df A data frame
#' @inheritParams row_out_df A data frame
#' @inheritParams exp_out_val An exposure value
#' @param chemicals Either "all" or "chemical_class"
#' @param aerial_ground TRUE / FALSE
#'
#' @return A nested data frame with two columns: The \code{row_out} column
#' contains the \code{exposure} data frame for the date range, and
#' \code{meta_data} contains the \code{meta_data} data frame for the date range.
#'
#' @importFrom dplyr %>%
help_calculate_exposure <- function(start_date, end_date, aerial_ground,
                              chemicals,
                              clean_pur_df,
                              location, pls_percents,
                              pur_filt, radius) {

  if (chemicals == "all") {
    if ("section" %in% colnames(pur_filt)) {
      if (aerial_ground) {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                              section, aerial_ground)
      } else {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                              section)
      }
    } else {
      if (aerial_ground) {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                              township, aerial_ground)
      } else {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date, township)
      }
    }
  } else {
    if ("section" %in% colnames(pur_filt)) {
      if (aerial_ground) {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                              section, chemical_class, aerial_ground)
      } else {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                              section, chemical_class)
      }
    } else {
      if (aerial_ground) {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                              township, chemical_class, aerial_ground)
      } else {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                              township, chemical_class)
      }
    }
  }

  buffer_area <- pi * (radius^2)

  if ("section" %in% colnames(pur_filt)) {
    exp <- help_write_md(clean_pur_df, pls_percents, pur_out, location, start_date,
                  end_date, radius, buffer_area,
                  MTRS, section)
  } else {
    exp <- help_write_md(clean_pur_df, pls_percents, pur_out, location, start_date,
                  end_date, radius, buffer_area,
                  MTR, township)
  }

  if (aerial_ground) {
    row_out <- help_return_exposure(start_date, end_date, location, radius, exp, buffer_area,
                          chemicals, aerial_ground)
  } else {
    row_out <- help_return_exposure(start_date, end_date, location, radius, exp, buffer_area,
                          chemicals)
  }

  nested_df <- list(row_out = list(row_out), meta_data = list(exp))
  attr(nested_df, "row.names") <- as.integer(1)
  class(nested_df) <- c("tbl_df", "data.frame")

  return(nested_df)

}

#' Return a map for a given exposure estimate.
#'
#' For a unique combination of time periods, chemicals, and application methods,
#' \code{help_map_exp} returns a plot showing amounts of pesticides applied for PLS
#' units intersecting with the buffer.
#'
#' This is a helper function for \code{map_exposure}.
#'
#' @inheritParams help_write_md
#' @param data_pls A data frame
#' @param gradient A character vector of hex color codes
#' @param location_longitude A numeric longitude value
#' @param location_latitude A numeric latitude value
#' @param buffer_or_county "buffer" or "county"
#' @param buffer_df A data frame
#' @param buffer2 A data frame
#' @param buffer A gpc.poly object
#' @param alpha A number in [0,1]
#' @param clean_pur A \code{clean_pur_df} data frame
#' @param pls_labels TRUE / FALSE
#' @param pls_labels_size A number
#' @param percentile A numeric vector
#' @param color_by "amount" or "percentile"
#'
#' @return A list with three elements:
#' \describe{
#'   \item{plot}{An exposure plot}
#'   \item{data}{A data frame with information about each PLS unit}{
#'   \item{cutoff_values}{A data frame with cutoff values returned if
#'   \code{color_by = "percentile"}
#' }
#'
#' @importFrom dplyr %>%
help_map_exp <- function(start_date, end_date, chemicals, aerial_ground,
                     none_recorded, data_pls,
                     gradient, location_longitude,
                     location_latitude, buffer_df, buffer2, buffer,
                     buffer_or_county, alpha, clean_pur, pls_labels,
                     pls_labels_size, percentile, color_by) {

  pls_df <- buffer_df %>%
    dplyr::right_join(data_pls, by = "pls")

  section_or_township <- unique(pls_df$section_or_township)

  if (section_or_township == "MTRS") {
    s_t <- "section"
  } else if (section_or_township == "MTR") {
    s_t <- "township"
  }

  legend_label <- paste0("Applied Pesticides\n(kg/", s_t, ")")

  if (!none_recorded) {

    full <- dplyr::filter(data_pls, percent > 0.999)
    full_pls_df <- buffer_df %>%
      dplyr::right_join(full, by = "pls") %>%
      dplyr::select(long, lat, group, kg_intersection) %>%
      dplyr::rename(kg = kg_intersection) %>%
      unique()

    partial <- dplyr::filter(data_pls, percent <= 0.999)
    partial_pls_df <- buffer_df %>%
      dplyr::right_join(partial, by = "pls")

    pls_partials <- unique(partial_pls_df$pls)

    if (length(pls_partials) != 0) {
      for (i in 1:length(pls_partials)) {

        df2 <- dplyr::filter(partial_pls_df, pls == pls_partials[i])

        pls <- dplyr::select(df2, long, lat)
        pls <- pls[grDevices::chull(pls), ]
        pls <- methods::as(pls, "gpc.poly")

        intersection <- raster::intersect(pls, buffer)

        int_df <- as.data.frame(methods::as(intersection, "matrix")) %>%
          dplyr::rename(long = x,
                        lat = y) %>%
          dplyr::mutate(group = paste0("int", i),
                        kg = unique(df2$kg_intersection))

        if (i == 1) {
          out_int <- int_df
        } else {
          out_int <- rbind(out_int, int_df)
        }
      }

      if (nrow(full_pls_df) != 0) {

        section_data <- rbind(out_int, full_pls_df)

      } else {

        section_data <- out_int

      }

    } else if (nrow(full_pls_df) != 0) {

      section_data <- full_pls_df

    }

    plot <- section_data %>%
      ggplot2::ggplot() +
      ggplot2::theme_void()

    if (color_by == "amount") {

      if (buffer_or_county == "buffer") {

        plot <- plot +
          geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = kg),
                       color = "black") +
          scale_fill_gradientn2(colours = gradient, alpha = alpha, name = legend_label,
                                na.value = "#FFFFFF")

      } else {

        ##

        if (!is.na(aerial_ground)) {
          clean_pur <- clean_pur %>%
            dplyr::filter(aerial_ground == aerial_ground)
        }

        if (chemicals != "all") {
          if ("chemical_class" %in% colnames(clean_pur)) {
            clean_pur <- clean_pur %>%
              dplyr::filter(chemical_class == chemicals)
          }
        }

        if (s_t == "section") {

          clean_pur2 <- clean_pur %>%
            dplyr::filter(date >= lubridate::ymd(start_date) &
                            date <= lubridate::ymd(end_date)) %>%
            dplyr::group_by(section) %>%
            dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE))

        } else if (s_t == "township") {

          clean_pur2 <- clean_pur %>%
            dplyr::filter(date >= lubridate::ymd(start_date) &
                            date <= lubridate::ymd(end_date)) %>%
            dplyr::group_by(township) %>%
            dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE))

        }

        clean_pur3 <- clean_pur2 %>%
          dplyr::mutate(source = "county")

        limits <- c(min(clean_pur3$kg, na.rm = TRUE),
                    max(clean_pur3$kg, na.rm = TRUE))

        ##
        plot <- plot +
          geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = kg),
                       color = "black") +
          scale_fill_gradientn2(colours = gradient, alpha = alpha, name = legend_label,
                                limits = limits, na.value = "#FFFFFF")
      }



    } else if (color_by == "percentile") {

      cutpoint_list <- help_categorize(section_data,
                                       buffer_or_county = buffer_or_county,
                                       start_date, end_date, aerial_ground,
                                       chemicals, clean_pur, s_t, percentile)

      cutoff_values <- cutpoint_list$cutoff_values

      section_data2 <- cutpoint_list$df

      categories <- cutpoint_list$categories

      if ("None recorded" %in% categories) {
        n_cols <-  as.integer(length(gradient)/(length(categories)-1))
        end_i <- length(categories)-1
      } else {
        n_cols <- as.integer(length(gradient)/length(categories))
        end_i <- length(categories)
      }
      for (i in 1:end_i) {
        col_vec <- gradient[n_cols*i]
        if (i == 1) {
          out <- col_vec
        } else {
          out <- c(out, col_vec)
        }
      }

      if ("None recorded" %in% categories) {
        out <- c(out, "#FFFFFF")
      }

      names(out) <- categories

      plot <- plot +
        ggplot2::geom_polygon(data = section_data2,
                              ggplot2::aes(x = long, y = lat, group = group,
                                           fill = category), color = "black") +
        ggplot2::scale_fill_manual(values = out, name = legend_label)

    }

    plot <- plot + ggplot2::geom_polygon(data = buffer_df,
                                         ggplot2::aes(x = long, y = lat, group = group),
                                         color ="black", fill = NA) +
      ggplot2::geom_point(x = location_longitude, y = location_latitude, size = 2)

    if (pls_labels) {

      df_all <- dplyr::select(pls_df, pls, DDLONG, DDLAT) %>% unique()

      plot <- plot +
        ggplot2::geom_text(data = df_all, ggplot2::aes(x = DDLONG, y = DDLAT,
                                                       label = pls),
                           size = pls_labels_size, fontface = "bold")

    }

    data_pls <- data_pls %>%
      dplyr::mutate(start_date = zoo::as.Date(start_date),
                    end_date = zoo::as.Date(end_date),
                    chemicals = chemicals, aerial_ground = aerial_ground) %>%
      dplyr::select(pls, percent, kg, kg_intersection, start_date, end_date,
                    chemicals, aerial_ground, none_recorded, location, radius,
                    area)

  } else {

    missing_buffer_df <- buffer_df %>% dplyr::mutate(perc_fill = "None recorded",
                                                     scale_fill = "0")

    if (color_by == "percentile") {

      missing_plot <- ggplot(missing_buffer_df) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = perc_fill), color = "black") +
        geom_point(x = location_longitude, y = location_latitude, color = "black",
                   size = 2) +
        theme_void() +
        scale_fill_manual(name = legend_label, values = c("None recorded" = NA))

    } else if (color_by == "amount") {

      missing_plot <- ggplot(missing_buffer_df) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = scale_fill), color = "black") +
        geom_point(x = location_longitude, y = location_latitude, color = "black",
                   size = 2) +
        theme_void() +
        scale_fill_manual(name = legend_label, values = c("0" = NA))

    }

    plot <- missing_plot
    start_date <- zoo::as.Date(start_date)
    end_date <- zoo::as.Date(end_date)
    data_pls <- data.frame(pls = "ALL", percent = NA,
                           kg = 0, kg_intersection = NA,
                           start_date = start_date,
                           end_date = end_date,
                           chemicals = NA,
                           aerial_ground = NA,
                           none_recorded = TRUE,
                           location = unique(data_pls$location),
                           radius = unique(data_pls$radius),
                           area = unique(data_pls$area))

    cutoff_values <- data.frame(percentile = percentile, kg = NA)

  }

  if (color_by == "amount") {
    return(list(plot = plot, data = data_pls))
  } else if (color_by == "percentile") {
    return(list(plot = plot, data = data_pls, cutoff_values = cutoff_values))
  }



}

#' Categorize a continuous scale by percentile cutpoints
#'
#' Given a data frame with a column with a continuous numeric variable,
#' \code{help_categorize} returns the data frame with a new column categorizing
#' that continuous variable by percentile cutpoints.
#'
#' This is a helper function for \code{help_map_exp}.
#'
#' @param section_data A data frame with a continuous numeric variable ("kg")
#' @param buffer_or_county Should cutpoints be determined by "county" or "buffer"?
#' @inheritParams help_write_md
#' @param clean_pur A \code{pull_clean_pur} data frame
#' @param s_t "section" or "township"
#' @param percentile A numeric vector in (0, 1).
#'
#' @return The input \code{section_data} data frame with an additional column
#' named \code{category}.
#'
#' @importFrom dplyr %>%
help_categorize <- function(section_data, buffer_or_county,
                           start_date = NULL, end_date = NULL,
                           aerial_ground = NULL, chemicals = NULL,
                           clean_pur = NULL, s_t = NULL, percentile) {

  if (buffer_or_county == "buffer") {

    perc <- as.data.frame(t(quantile(unique(section_data$kg),
                                     probs = percentile, na.rm = TRUE)))
    vec <- 0
    for (i in 1:length(percentile)) {
      vec <- c(vec, perc[, i])
    }
    vec <- c(vec, max(unique(section_data$kg),
                      na.rm = TRUE))
    perc_numbers <- as.character(percentile * 100)
    first <- paste0("<=", perc_numbers[1], "th percentile")
    last <- paste0(">=", perc_numbers[length(perc_numbers)], "th")

    for (i in 1:(length(perc_numbers) - 1)) {
      label <- paste0(">=", perc_numbers[i], "th to <", perc_numbers[i+1], "th")
      if (i == 1) {
        middle <- label
      } else {
        middle <- c(middle, label)
      }
    }

    labels <- c(first, middle, last)

    df_out <- section_data %>%
      dplyr::mutate(category = as.character(cut(section_data$kg, vec, labels = labels)),
                    category = ifelse(is.na(category), "None recorded", category))

    if ("None recorded" %in% unique(df_out$category)) {
      df_out$category <- factor(df_out$category, levels = c(labels, "None recorded"))
    } else {
      df_out$category <- factor(df_out$category, levels = labels)
    }

  } else if (buffer_or_county == "county") {

    if (!is.na(aerial_ground)) {
      clean_pur <- clean_pur %>%
        dplyr::filter(aerial_ground == aerial_ground)
    }

    if (chemicals != "all") {
      if ("chemical_class" %in% colnames(clean_pur)) {
        clean_pur <- clean_pur %>%
          dplyr::filter(chemical_class == chemicals)
      }
    }

    if (s_t == "section") {

      clean_pur2 <- clean_pur %>%
        dplyr::filter(date >= lubridate::ymd(start_date) &
                        date <= lubridate::ymd(end_date)) %>%
        dplyr::group_by(section) %>%
        dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE))

    } else if (s_t == "township") {

      clean_pur2 <- clean_pur %>%
        dplyr::filter(date >= lubridate::ymd(start_date) &
                        date <= lubridate::ymd(end_date)) %>%
        dplyr::group_by(township) %>%
        dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE))

    }

    clean_pur3 <- clean_pur2 %>%
      dplyr::mutate(source = "county")

    section_data <- section_data %>%
      dplyr::mutate(source = "buffer")

    to_join <- section_data %>%
      dplyr::select(group, kg, source)

    all_pls <- clean_pur3 %>%
      dplyr::full_join(to_join, by = c("kg", "source"))

    perc <- as.data.frame(t(quantile(unique(all_pls$kg),
                                     probs = percentile, na.rm = TRUE)))
    vec <- 0
    for (i in 1:length(percentile)) {
      vec <- c(vec, perc[, i])
    }
    vec <- c(vec, max(unique(all_pls$kg),
                      na.rm = TRUE))
    perc_numbers <- as.character(percentile * 100)
    first <- paste0("<=", perc_numbers[1], "th percentile")
    last <- paste0(">=", perc_numbers[length(perc_numbers)], "th")

    for (i in 1:(length(perc_numbers) - 1)) {
      label <- paste0(">=", perc_numbers[i], "th to <", perc_numbers[i+1], "th")
      if (i == 1) {
        middle <- label
      } else {
        middle <- c(middle, label)
      }
    }

    labels <- c(first, middle, last)

    all_pls_out <- all_pls %>%
      dplyr::mutate(category = as.character(cut(all_pls$kg, vec, labels = labels)),
                    category = ifelse(is.na(category), "None recorded", category))

    df_out2 <- section_data %>%
      dplyr::mutate(category = as.character(cut(section_data$kg, vec, labels = labels)),
                    category = ifelse(is.na(category), "None recorded", category))

    buffer_pls <- df_out2 %>%
      dplyr::filter(source == "buffer") %>%
      dplyr::select(kg, source, group, category) %>%
      dplyr::full_join(section_data, c("kg", "source", "group")) %>%
      dplyr::select(long, lat, group, kg, category)

    if ("None recorded" %in% unique(buffer_pls$category)) {
      buffer_pls$category <- factor(buffer_pls$category, levels = c(labels, "None recorded"))
    } else {
      buffer_pls$category <- factor(buffer_pls$category, levels = labels)
    }

    df_out <- buffer_pls

  }

  if ("None recorded" %in% unique(df_out$category)) {
    labels <- c(labels, "None recorded")
  }

  percents <- sub("%", "", colnames(perc)) %>% as.numeric() / 100
  cutoff <- perc %>% tidyr::gather("percentile", "kg") %>%
    dplyr::mutate(percentile = percents)

  out <- list(df = df_out, categories = labels, cutoff_values = cutoff)

  return(out)

}
