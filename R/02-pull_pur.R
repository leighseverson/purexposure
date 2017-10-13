#' Pull raw PUR file for a single year and a county or counties.
#'
#' \code{pull_pur_file} pulls the raw PUR dataset for a particular year and
#' saves datasets for specified counties in a data frame.
#'
#' @inheritParams pull_raw_pur
#' @param counties A character string giving either county names or two digit
#'  county codes. Not case sensitive. California names and county codes as they
#'  appear in PUR datasets can be found in the county_codes dataset available
#'  with this package. For example, to return data for Alameda county, enter
#'  either "alameda" or "01" for the county argument. \code{counties = "all"}
#'  will pull data for all 58 California counties.
#'
#' @return A data frame with 33 columns. Counties are indicated by
#'   \code{county_cd}; the year for which data was pulled is indicated by
#'   \code{applic_dt}.
#'
#' \dontrun{
#' raw_file <- pull_pur_file(1999, c("40", "ventura", "yuba"))
#' raw_file <- pull_pur_file(2015, "all")
#' }
#' @export
pull_pur_file <- function(year, counties = "all", download_progress = FALSE) {

  current_dir <- getwd()

  url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
                year, ".zip")
  file <- paste0("pur", year, ".zip")

  dir <- tempdir()
  setwd(dir)

  if (download_progress) {
    quiet <- FALSE
  } else {
    quiet <- TRUE
  }

  download.file(url, destfile = file, mode = "wb", quiet = quiet)
  unzip(file, exdir = dir)

  sm_year <- substr(year, 3, 4)


  if (!"all" %in% counties) {
    codes <- find_counties(counties)

    read_in_counties <- function(code) {
      raw_data <- suppressWarnings(suppressMessages(
        readr::read_csv(paste0("udc", sm_year, "_", code, ".txt"))
      ))
      raw_data <- dplyr::mutate_all(raw_data, as.character)
      return(raw_data)
    }

    counties_in_year <- purrr::map_dfr(codes, read_in_counties) %>%
      dplyr::arrange(applic_dt, county_cd)

  } else {
    files <- grep(paste0("udc", sm_year, "_"), list.files(), value = TRUE)

    read_in_counties2 <- function(file) {
      raw_data <- suppressWarnings(suppressMessages(
        readr::read_csv(file)
      ))
      raw_data <- dplyr::mutate_all(raw_data, as.character)
      return(raw_data)
    }

    counties_in_year <- purrr::map_dfr(files, read_in_counties2) %>%
      dplyr::arrange(applic_dt, county_cd)

  }

  setwd(current_dir)
  return(counties_in_year)

}

#' Pull raw PUR data by counties and years
#'
#' \code{pull_raw_pur} pulls a raw PUR dataset for a given year and vector of
#'   California counties.
#'
#' @param years A four-digit numeric year or vector of years in the range of
#'   1990 to 2015. Indicates the years for which you would like to pull PUR
#'   datasets. \code{years == "all"} will pull data from 1990 through 2015.
#' @param counties A vector of character strings giving either a county name or
#'   a two digit county code for each county. Not case sensitive. California names
#'   and county codes as they appear in PUR datasets can be found in the
#'   \code{county_codes} dataset available with this package. For example, to
#'   return data for Alameda county, enter either "alameda" or "01" for the
#'   \code{counties} argument. \code{counties = "all"} will return data for
#'   all 58 California counties.
#' @param verbose TRUE / FALSE indicating whether you would like a single message
#'   printed indicating which counties and years you are pulling data for. The
#'   default value is TRUE.
#' @param download_progress TRUE / FALSE indicating whether you would like a
#'   message and progress bar printed for each year of PUR data that is downloaded.
#'   The default value is FALSE.
#'
#' @return A data frame with 33 columns. Different years and counties for which
#'   data was pulled are indicated by \code{applic_dt} and \code{county_cd},
#'   respectively.
#'
#' @section Note: For documentation of raw PUR data, see the Pesticide Use
#'   Report Data User Guide & Documentation document published by the California
#'   Department of Pesticide Regulation. This file is saved as "cd_doc.pdf" in any
#'   "pur[year].zip" file between 1990 and 2015 found here:
#'   \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/}.
#'
#' @examples
#' \dontrun{
#' df <- pull_raw_pur(download_progress = TRUE)
#' df2 <- pull_raw_pur(years = c(2000, 2010), counties = c("butte", "15", "01"))
#' df3 <- pull_raw_pur(years = 2015, counties = c("colusa"))
#' }
#' @importFrom dplyr %>%
#' @export
pull_raw_pur <- function(years = "all", counties = "all", verbose = TRUE,
                         download_progress = FALSE) {

  if (!all(is.numeric(years))) {
    stop("Years should be four-digit numeric values.")
  }
  if (all(is.numeric(years)) & (min(years) < 1990 | max(years) > 2015)) {
    stop("Years should be between 1990 and 2015.")
  }
  if ("all" %in% tolower(years)) {
    years <- 1990:2015
  }

  code_df <- purexposure::county_codes

  ## years section
  if (length(years) == 1) {
    year_message <- paste0(years, ".")
  } else if (length(years) == 2) {
    year_message <- paste0(years[1], " and ", years[2], ".")
  } else if (length(years) > 1) {
    years_list <- split(years, cumsum(c(1, diff(years) != 1)))
    if (length(years_list) == 1) {
      year_message <- paste0(years[1], " through ", years[length(years)], ".")
    } else {
      years_vec <- years[1:length(years)-1]
      years_vec <- paste(years_vec, collapse = ", ")
      year_message <- paste0(years_vec, ", and ", years[length(years)], ".")
    }
  }

  if (!"all" %in% counties) {
    ## error handling

    if (!all(is.character(counties))) {
      stop("County names and/or codes should be character strings.")
    }

    names_clean <- find_counties(counties, return = "names")

    ## messaging

    ## counties section
    if (length(names_clean) == 1) {
      county_message <- paste0(names_clean, " county")
    } else if (length(names_clean) == 2) {
      county_message <- paste0(names_clean[1], " and ", names_clean[2],
                               " counties")
    } else if (length(names_clean) > 2) {
      counties_vec <- names_clean[1:length(names_clean)-1]
      counties_vec <- paste(counties_vec, collapse = ", ")
      county_message <- paste0(counties_vec, ", and ",
                               names_clean[length(names_clean)], " counties")
    }

  } else {
    county_message <- "all counties"
  }

  if (verbose) {
    message(paste0("Pulling PUR data for ", county_message, " for ",
                   year_message, " Great choice!"))
  }

  ## pull data

  tibble_to_vector <- function(tib) {
    vec <- tib %>% dplyr::pull(1) %>% as.character()
    return(vec)
  }

  if (!"all" %in% counties) {
    years_counties <- expand.grid(year = years, county = counties) %>%
      dplyr::group_by(year) %>%
      tidyr::nest() %>%
      dplyr::mutate(counties = purrr::map(data, tibble_to_vector))

    raw_df <- purrr::map2_dfr(years_counties$year,
                              years_counties$counties,
                              pull_pur_file,
                              download_progress = download_progress)
  } else {

    raw_df <- purrr::map_dfr(years, pull_pur_file, counties = "all",
                             download_progress = download_progress)

  }

  return(raw_df)

}
