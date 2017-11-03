#' Pull raw PUR file for a single year and a county or counties.
#'
#' \code{pull_pur_file} pulls the raw PUR dataset for a particular year and
#' saves datasets for specified counties in a data frame.
#'
#' @inheritParams pull_raw_pur
#' @param counties A character vector giving either county names or two digit
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
#' @section Note:
#' If this function returns an error (because the FTP site is down, for
#' example), check your working directory. You may want to change it back from a
#' temporary directory.
#'
#' \dontrun{
#' raw_file <- pull_pur_file(1999, c("40", "ventura", "yuba"))
#' raw_file <- pull_pur_file(2015, "all")
#' }
#' @importFrom magrittr %>%
#' @export
pull_pur_file <- function(year, counties = "all", download_progress = TRUE) {

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

    codes <- find_counties(counties[[1]])

    counties_in_year <- purrr::map_dfr(codes, help_read_in_counties, type = "codes",
                                       year = year) %>%
      dplyr::arrange(applic_dt, county_cd)

  } else {

    files <- grep(paste0("udc", sm_year, "_"), list.files(), value = TRUE)

    counties_in_year <- purrr::map_dfr(files, help_read_in_counties, type = "files",
                                       year = year) %>%
      dplyr::arrange(applic_dt, county_cd)

  }

  setwd(current_dir)

  return(counties_in_year)

}

#' Pull raw PUR data by counties and years.
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
#'   The default value is TRUE.
#'
#' @return A data frame with 33 columns. Different years and counties for which
#'   data was pulled are indicated by \code{applic_dt} and \code{county_cd},
#'   respectively.
#'
#' @section Note:
#'  \itemize{
#'    \item{For documentation of raw PUR data, see the Pesticide Use
#'   Report Data User Guide & Documentation document published by the California
#'   Department of Pesticide Regulation. This file is saved as "cd_doc.pdf" in any
#'   "pur[year].zip" file between 1990 and 2015 found here:
#'   \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/}.}
#'   \item{If this function returns an error (because the FTP site is down, for
#'   example), check your working directory. You may want to change it back from
#'   a temporary directory.}
#' }
#'
#' @examples
#' \dontrun{
#' df <- pull_raw_pur(download_progress = TRUE) # this will take a while to run
#' df2 <- pull_raw_pur(years = c(2000, 2010), counties = c("butte", "15", "01"))
#' df3 <- pull_raw_pur(years = 2015, counties = c("colusa"))
#' }
#' @importFrom magrittr %>%
#' @export
pull_raw_pur <- function(years = "all", counties = "all", verbose = TRUE,
                         download_progress = TRUE) {

  if ("all" %in% tolower(years)) {
    years <- 1990:2015
  }
  if (!all(is.numeric(years))) {
    stop("Years should be four-digit numeric values.")
  }
  if (all(is.numeric(years)) & (min(years) < 1990 | max(years) > 2015)) {
    stop("Years should be between 1990 and 2015.")
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

  if (!"all" %in% counties) {

    years_counties <- expand.grid(year = years, county = counties) %>%
      dplyr::group_by(year) %>%
      tidyr::nest() %>%
      dplyr::mutate(counties = purrr::map(data, tibble_to_vector))

    for (i in 1:nrow(years_counties)) {
      df <- purrr::map2_dfr(years_counties$year[[i]],
                            years_counties$counties[[i]],
                            pull_pur_file, download_progress = download_progress)
      if (i == 1) {
        out <- df
      } else {
        out <- rbind(out, df)
      }
    }

    raw_df <- out


  } else {

    raw_df <- purrr::map_dfr(years, pull_pur_file, counties = "all",
                             download_progress = download_progress)

  }

  return(raw_df)

}

#' Pull cleaned PUR data by counties, years, and active ingredients.
#'
#' \code{pull_clean_pur} returns a data frame of cleaned Pesticide Use Report data
#' filtered by counties, years, and active ingredients. Active ingredients
#' or chemical classes present in applied pesticides can be summed by either
#' Public Land Survey (PLS) section or township.
#'
#' @inheritParams pull_raw_pur
#' @param chemicals A string or vector of strings giving search terms of
#'   chemicals to match with active ingredients present in pesticides applied in
#'   the given years. The default value is "all", which returns records for all
#'   active ingredients applied in a given year. See the CDPR's Summary of PUR
#'   Data document here:
#'   \url{http://www.cdpr.ca.gov/docs/pur/pur08rep/chmrpt08.pdf} for
#'   comprehensive classifications of active ingredients.
#' @param sum_application TRUE / FALSE indicating if you would like to sum the
#'   amounts of applied active ingredients by day, the geographic unit
#'   given in \code{unit}, and by either active ingredients or chemical class
#'   (indicated by \code{sum} and \code{chemical_class}). The default value
#'   is FALSE.
#' @param unit A character string giving either "section" or "township".
#'   Specifies whether applications of each active ingredient should be summed
#'   by California section (the default) or by township. Only used if
#'   \code{sum_application} is \code{TRUE}.
#' @param sum A character string giving either "all" (the
#'   default) or "chemical_class". If \code{sum_application = TRUE},
#'   \code{sum} indicates whether you would like to sum across all active
#'   ingredients, giving an estimation of the total pesticides applied in a
#'   given section or township ("all"), or by a chemical class specified in a
#'   data frame given in the argument \code{chemical_class}.
#' @param chemical_class A data frame with only three columns: \code{chem_code},
#'   \code{chemname}, and \code{chemical_class}. \code{chem_code} should have
#'   integer values giving PUR chemical codes, and \code{chemname} should have
#'   character strings with corresponding PUR chemical names (these can be
#'   searched for using the \code{find_chemical_codes} function or with the
#'   \code{chemical_list} dataset included with this package). The
#'   \code{chemical_class} column should have character strings indicating the
#'   chemical class corresponding to each \code{chem_code}. The
#'   \code{chemical_class} for a group of active ingredients should be decided
#'   upon by the user. Only used if \code{sum = "chemical_class"}. See
#'   the CDPR's Summary of PUR Data document here:
#'   \url{http://www.cdpr.ca.gov/docs/pur/pur08rep/chmrpt08.pdf} for
#'   comprehensive classifications of active ingredients.
#' @param aerial_ground TRUE / FALSE indicating if you would like to
#'   retain aerial/ground application data ("A" = aerial, "G" = ground, and
#'   "O" = other.) The default is FALSE.
#' @param raw_pur_df A raw PUR data frame. Optional. If you've already downloaded
#'   a raw PUR data frame using \code{pull_raw_pur}, this argument prevents
#'   \code{pull_clean_pur} from downloading the same data again.
#'
#' @return A data frame with 13 columns:
#'   \describe{
#'     \item{chem_code}{An integer value giving the PUR chemical code
#'     for the active ingredient applied. Not included if
#'     \code{sum_application = TRUE} and \code{sum = "chemical_class"}.}
#'     \item{chemname}{A character string giving PUR chemical active
#'     ingredient names. Unique values of \code{chemname} are matched with terms
#'     provided in the \code{chemicals} argument. Not included
#'     if \code{sum_application = TRUE} and \code{sum = "chemical_class"}.}
#'     \item{chemical_class}{If \code{sum_application = TRUE} and
#'     \code{sum = "chemical_class"}, this column will give values of the
#'     \code{chemical_class} column in the input \code{chemical_class} data frame.
#'     If there are active ingredients pulled based on the
#'     \code{chemicals} argument that are not present in the \code{chemical_class}
#'     data frame, these chemicals will be summed under the class "other".}
#'     \item{kg_chm_used}{A numeric value giving the amount of the active
#'     ingredient applied (kilograms).}
#'     \item{section}{A string nine characters long indicating the section
#'     of application. PLS sections are uniquely identified by a combination of
#'     base line meridian (S, M, or H), township (01-48), township direction
#'     (N or S), range (01-47), range direction (E or W) and section number
#'     (01-36). This column is not included if
#'     \code{sum_application = TRUE} and \code{unit = "township"}.}
#'     \item{township}{A string 7 characters long indicating the township
#'     of application. PLS townships are uniquely identified by a combination of
#'     base line meridian (S, M, or H), township (01-48), township direction
#'     (N or S), range (01-47), and range direction (E or W).}
#'     \item{county_name}{A character string giving the county name where
#'     application took place.}
#'     \item{county_code}{A string two characters long giving the PUR county
#'     code where application took place.}
#'     \item{date}{The date of application (yyyy-mm-dd).}
#'     \item{aerial_ground}{A character giving the application method.
#'     "A" = aerial, "G" = ground, and "O" = other. Not included
#'     if \code{aerial_ground = FALSE}.}
#'     \item{use_no}{A character string identifing unique application of an
#'     active ingredient across years. This value is a combination of the raw PUR
#'     \code{use_no} column and the year of application. Will have values of
#'     \code{NA} if \code{sum_appliction = TRUE}.}
#'     \item{outlier}{A logical value indicating whether the
#'     amount listed in \code{kg_chm_used} has been corrected large amounts
#'     entered in error. The algorithm for identifying and replacing outliers
#'     was developed based on methods used by Gunier et al. (2001). Please see
#'     the package vignette for more detail regarding these methods. Will have
#'     values of \code{NA} if \code{sum_application = TRUE}.}
#'     \item{prodno}{Integer. The California Registration Number for the applied
#'     pesticide (will be repeated for different active ingredients present in
#'     the product). You can match product registration numbers with product
#'     names, which can be pulled using the \code{pull_product_table} function.
#'     This column is not returned if \code{sum_application = TRUE}.}
#'  }
#'
#' @section Note:
#'   \itemize{
#'     \item The \code{chemical_list} data frame for a particular year lists
#'     active ingredients present in applied pesticides across the state of
#'     California. Therefore, PUR data for a particular county may not include
#'     records for active ingredients listed in the \code{chemical_list} dataset
#'     for the same year.
#'     \item To pull raw PUR data, see the \code{pull_raw_pur} function.
#'     For documentation of raw PUR data, see the Pesticide Use Report Data User
#'     Guide & Documentation document published by the California Department of
#'     Pesticide Regulation. This file is saved as "cd_doc.pdf" in
#'     any "pur[year].zip" file between 1990 and 2015 found here:
#'     \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/}.
#'     \item{If this function returns an error, check your working directory.
#'     You may want to change it back from a temporary directory.}
#' }
#'
#' @examples
#' \dontrun{
#' df <- pull_clean_pur(download_progress = TRUE)
#' df2 <- pull_clean_pur(years = 2000:2010,
#'                       counties = c("01", "nevada", "riverside"),
#'                       chemicals = "methylene",
#'                       aerial_ground = TRUE)
#'
#' # filter to particular products
#' prod_nos <- find_product_name(2003, "insecticide") %>%
#'     dplyr::select(prodno) %>%
#'     tibble_to_vector()
#'
#' df3 <- pull_clean_pur(2003) %>%
#'     dplyr::filter(prodno %in% prod_nos)
#'
#' # Sum application by active ingredients
#' df4 <- pull_clean_pur(years = 2000:2010,
#'                       counties = c("01", "nevada", "riverside"),
#'                       chemicals = "methylene",
#'                       unit = "township", sum_application = TRUE)
#'
#' # Or by chemical classes
#' chemical_class_df <- rbind(find_chemical_codes(2000, "methylene"),
#'                            find_chemical_codes(2000, "aldehyde")) %>%
#'    dplyr::rename(chemical_class = chemical)
#'
#' df5 <- pull_clean_pur(years = 1995,
#'                       counties = "fresno",
#'                       chemicals = chemical_class_df$chemname,
#'                       sum_application = TRUE,
#'                       sum = "chemical_class",
#'                       unit = "township",
#'                       chemical_class = chemical_class_df)
#'
#' # clean an existing raw PUR dataset
#' placer_05 <- pull_raw_pur(2005, "placer")
#' df6 <- pull_clean_pur(raw_pur_df = placer_05)
#' }
#' @importFrom magrittr %>%
#' @export
pull_clean_pur <- function(years = "all", counties = "all", chemicals = "all",
                           sum_application = FALSE, unit = "section",
                           sum = "all", chemical_class = NULL,
                           aerial_ground = TRUE, verbose = TRUE,
                           download_progress = TRUE,
                           raw_pur_df = NULL) {

  if (is.null(raw_pur_df)) {
    raw_df <- pull_raw_pur(years = years, counties = counties, verbose = verbose,
                           download_progress = download_progress)
  } else {

    check <- colnames(raw_pur_df) == c("use_no", "prodno", "chem_code",
                                      "prodchem_pct", "lbs_chm_used",
                                      "lbs_prd_used", "amt_prd_used",
                                      "unit_of_meas", "acre_planted",
                                      "unit_planted", "acre_treated",
                                      "unit_treated", "applic_cnt", "applic_dt",
                                      "applic_time", "county_cd", "base_ln_mer",
                                      "township", "tship_dir", "range",
                                      "range_dir", "section", "site_loc_id",
                                      "grower_id", "license_no", "planting_seq",
                                      "aer_gnd_ind", "site_code", "qualify_cd",
                                      "batch_no", "document_no", "summary_cd",
                                      "record_id")

    if (!all(check)) {
      stop(paste0("The raw_pur_df data frame should be returned from ",
                  "pull_raw_pur() and should have 33 columns."))
    }

    unique_years <- raw_pur_df %>%
      dplyr::mutate(year = lubridate::year(lubridate::ymd(applic_dt))) %>%
      dplyr::select(year) %>%
      unique() %>%
      tibble_to_vector()

    unique_counties <- raw_pur_df %>%
      dplyr::select(county_cd) %>%
      unique() %>%
      tibble_to_vector() %>%
      find_counties(return = "codes")

    if (is.character(years)) {
      if (years == "all") {
      years_check <- unique_years
      }
    } else {
      years_check <- years
    }

    if (counties == "all") {
      counties_check <- unique_counties
    } else {
      counties_check <- find_counties(counties)
    }

    years_check <- years_check %in% unique_years

    if (!all(years_check)) {
      stop(paste0("The years argument specifies years not present in the given ",
                  "raw_pur_df data frame."))
    }

    counties_check <- counties_check %in% unique_counties

    if (!all(counties_check)) {
      stop(paste0("The counties argument specifies counties not present in the ",
                  "given raw_pur_df data frame."))
    }

    raw_df <- raw_pur_df
  }

  df <- raw_df %>%
    dplyr::mutate(township_pad = stringr::str_pad(raw_df$township, 2, "left", pad = "0"),
                  range = stringr::str_pad(raw_df$range, 2, "left", pad = "0"),
                  section = stringr::str_pad(raw_df$section, 2, "left", pad = "0"),
                  MTRS = as.character(paste0(base_ln_mer, township_pad, tship_dir,
                                             range, range_dir, section)),
                  township = as.character(paste0(base_ln_mer, township_pad, tship_dir)),
                  MTR = as.character(paste0(township, range, range_dir))) %>%
    dplyr::select(chem_code, lbs_chm_used, MTRS, MTR, county_cd, applic_dt,
                  aer_gnd_ind, use_no, acre_treated, unit_treated, prodno) %>%
    dplyr::mutate(unit_treated = as.factor(unit_treated)) %>%
    dplyr::filter(!MTRS %in% c(".0..0..0.", ".00.00.00", "NANANANANANA"),
                  unit_treated %in% c("A", "S")) %>%
    dplyr::mutate(acre_treated = as.numeric(acre_treated),
                  lbs_chm_used = as.numeric(lbs_chm_used),
                  acre_treated = ifelse(unit_treated == "S",
                                        acre_treated * 2.29568e-5,
                                        acre_treated),
                  unit_treated = "A", # going to remove this later - all acres
                  lbs_per_acre = lbs_chm_used/acre_treated,
                  chem_code = as.integer(chem_code))
  # correct for outliers (Gunier et al. (2001))
  # they used 1995 - I'm doing this for each year
  # if lbs per acre is larger than the calculated max rate of lbs per acre
  # (mean rate plus 2 standard deviations) then lbs_chm_used is replaced
  # with the calculated max rate multiplied by number of acres treated (acre_treated)
  calc_max <- df %>%
    dplyr::mutate(year = as.character(lubridate::year(lubridate::ymd(applic_dt)))) %>%
    dplyr::group_by(chem_code, year) %>%
    dplyr::summarize(mean = mean(lbs_per_acre, na.rm = TRUE),
                     sd = sd(lbs_per_acre, na.rm = TRUE)) %>%
    dplyr::mutate(calc_max = mean + 2*sd)

  ## filter active ingredients, add chemname column

  if (!"all" %in% chemicals) {

    years_chemicals <- expand.grid(year = years, chemicals = chemicals) %>%
      dplyr::group_by(year) %>%
      tidyr::nest() %>%
      dplyr::mutate(chemicals = purrr::map(data, tibble_to_vector))

    chem_df <- purrr::map2_dfr(years_chemicals$year, years_chemicals$chemicals,
                               find_chemical_codes) %>% unique()

    df <- df %>%
      dplyr::filter(chem_code %in% chem_df$chem_code) %>%
      dplyr::left_join(chem_df, by = "chem_code") %>%
      dplyr::select(-chemical) %>%
      dplyr::mutate(applic_dt = lubridate::ymd(applic_dt))

    # chemicals
    if (nrow(df) == 0) {
      if (length(chemicals) == 1) {
        chem_message <- paste0(chemicals)
      } else if (length(chemicals) == 2) {
        chem_message <- paste0(chemicals[1], " or ", chemicals[2])
      } else if (length(chemicals) > 2 & length(chemicals) < 11) {
        chems_vec <- chemicals[1:length(chemicals)-1]
        chems_vec <- paste(chems_vec, collapse = ", ")
        chem_message <- paste0(chems_vec, ", or ", chemicals[length(chemicals)])
      } else if (length(chemicals) >= 11) {
        chem_message <- "these chemicals"
      }

      # years
      if (length(years) == 1) {
        year_message <- years
      } else if (length(years) == 2) {
        year_message <- paste0(years[1], " or ", years[2])
      } else if (length(years) > 1) {
        years_list <- split(years, cumsum(c(1, diff(years) != 1)))
        if (length(years_list) == 1) {
          year_message <- paste0(years[1], " through ", years[length(years)])
        } else {
          years_vec <- years[1:length(years)-1]
          years_vec <- paste(years_vec, collapse = ", ")
          year_message <- paste0(years_vec, ", or ", years[length(years)])
        }
      }

      # counties
      names_clean <- find_counties(counties, return = "names")

      if (length(names_clean) == 1) {
        county_message <- paste0(names_clean, " county.")
      } else if (length(names_clean) == 2) {
        county_message <- paste0(names_clean[1], " or ", names_clean[2],
                                 " county.")
      } else if (length(names_clean) > 2) {
        counties_vec <- names_clean[1:length(names_clean)-1]
        counties_vec <- paste(counties_vec, collapse = ", ")
        county_message <- paste0(counties_vec, ", or ",
                                 names_clean[length(names_clean)], " county.")
      }

      stop(paste0("There weren't any pesticides containing ", chem_message,
                  " applied in ", year_message, " in ", county_message))
    }

  } else {

    if (is.character(years)) {
      if (years == "all") {
        years <- 1990:2015
      }
    }

    chem_df <- purexposure::chemical_list
    out_chem_list <- list()
    for (i in 1:length(years)) {
      chem_year <- chem_df[[as.character(years[i])]]
      chem_year <- chem_year %>% dplyr::mutate(year = as.character(years[i]))
      out_chem_list[[i]] <- chem_year
    }
    out_chem_df <- dplyr::bind_rows(out_chem_list)

    df <- df %>% dplyr::mutate(applic_dt = lubridate::ymd(applic_dt),
                               year = lubridate::year(applic_dt),
                               year = as.character(year)) %>%
      dplyr::left_join(out_chem_df, by = c("chem_code", "year"))

  }

  df <- df %>% dplyr::mutate(year = as.character(lubridate::year(applic_dt)))

  df2 <- calc_max %>%
    dplyr::select(chem_code, year, calc_max) %>%
    dplyr::right_join(df, by = c("chem_code", "year")) %>%
    dplyr::mutate(outlier = ifelse((!is.na(calc_max) &
                                      lbs_per_acre > calc_max), TRUE, FALSE),
                  lbs_chm_used = ifelse(lbs_per_acre > calc_max,
                                        calc_max*acre_treated, lbs_chm_used)) %>%
    dplyr::rename(county_code = county_cd) %>%
    dplyr::ungroup()

  county <- purexposure::county_codes

  out <- county %>%
    dplyr::right_join(df2, by = "county_code") %>%
    dplyr::mutate(use_no = paste0(use_no, "_", lubridate::year(applic_dt)),
                  kg_chm_used = lbs_chm_used/2.20562) %>%
    dplyr::select(chem_code, chemname, kg_chm_used, MTRS, MTR, county_name,
                  county_code, applic_dt, aer_gnd_ind, use_no, outlier, prodno) %>%
    dplyr::rename(section = MTRS,
                  township = MTR,
                  date = applic_dt,
                  aerial_ground = aer_gnd_ind) %>%
    dplyr::arrange(date, county_name)

  # missing section and township IDs

  missing_sections <- c(grep("\\?", out$section, value = TRUE),
                        grep("000000000", out$section, value = TRUE))
  missing_townships <- c(grep("\\?", out$township, value = TRUE),
                         grep("0000000", out$township, value = TRUE))

  if (length(missing_sections) != 0) {
    out <- out %>% dplyr::mutate(section = ifelse(section %in% missing_sections,
                                                  NA, section),
                                 section = ifelse(section == "000000000",
                                                  NA, section))
  }

  if (length(missing_townships) != 0) {
    out <- out %>% dplyr::mutate(township = ifelse(township %in% missing_sections,
                                                   NA, township),
                                 township = ifelse(township == "000000",
                                                   NA, section))
  }

  if (sum_application) {

    section_townships <- out %>%
      dplyr::select(section, township) %>%
      unique()

    if (sum == "all") {
      if (unit == "section") {
        if (aerial_ground) {
          out <- help_sum_application(out, "all", "section", TRUE,
                                    section_townships,
                                    chem_code,
                                    chemname, section, county_name, county_code,
                                    date, aerial_ground)
        } else {
          out <- help_sum_application(out, "all", "section", FALSE,
                                    section_townships,
                                    chem_code,
                                    chemname, section, county_name, county_code,
                                    date)
        }
      } else if (unit == "township") {
        if (aerial_ground) {
          out <- help_sum_application(out, "all", "township", TRUE,
                                    section_townships,
                                    chem_code,
                                    chemname, township, county_name, county_code,
                                    date, aerial_ground)
        } else {
          out <- help_sum_application(out, "all", "township", FALSE,
                                    section_townships,
                                    chem_code,
                                    chemname, township, county_name, county_code,
                                    date)
        }
      }
    } else if (sum == "chemical_class") {

      ## error handling for chemical_class df
      if (!is.null(chemical_class) & !is.data.frame(chemical_class)) {
        stop("The chemical_class argument should be a data frame.")
      }
      if (!is.null(chemical_class) & is.data.frame(chemical_class) &
          !all(colnames(chemical_class) == c("chem_code", "chemname", "chemical_class"))) {
        stop(writeLines(paste0("The data frame entered in the chemical class ",
                               "argument should have only three columns named chem_code, ",
                               "chemname, and chemical_class.")))
      }
      if (!is.null(chemical_class) & is.data.frame(chemical_class) &
          all(colnames(chemical_class) == c("chem_code", "chemname", "chemical_class")) &
          !is.integer(chemical_class$chem_code)) {
        stop("The chem_code column should have integer values.")
      }
      if (!is.null(chemical_class) & is.data.frame(chemical_class) &
          all(colnames(chemical_class) == c("chem_code", "chemname", "chemical_class")) &
          !is.character(chemical_class$chemname)) {
        stop("The chemname column should have character values.")
      }
      if (!is.null(chemical_class) & is.data.frame(chemical_class) &
          all(colnames(chemical_class) == c("chem_code", "chemname", "chemical_class")) &
          !is.character(chemical_class$chemical_class)) {
        stop("The chemical_class column should have character values.")
      }

      if (unit == "section") {
        if (aerial_ground) {
          out <- help_sum_application(out, "chemical_class", "section", TRUE,
                                    section_townships,
                                    chemical_class = chemical_class,
                                    chemical_class, section, county_name,
                                    county_code, date, aerial_ground)
        } else {
          out <- help_sum_application(out, "chemical_class", "section", FALSE,
                                    section_townships,
                                    chemical_class = chemical_class,
                                    chemical_class, section, county_name,
                                    county_code, date)
        }
      } else if (unit == "township") {
        if (aerial_ground) {
          out <- help_sum_application(out, "chemical_class", "township", TRUE,
                                    section_townships,
                                    chemical_class = chemical_class,
                                    chemical_class, township, county_name,
                                    county_code, date, aerial_ground)
        } else {
          out <- help_sum_application(out, "chemical_class", "township", FALSE,
                                    section_townships,
                                    chemical_class = chemical_class,
                                    chemical_class, township, county_name,
                                    county_code, date)
          }
      }
    }
  } else {
    if (!aerial_ground) {
      out <- out %>% dplyr::select(-aerial_ground)
    }
  }

  if ("prodno" %in% colnames(out)) {
    out <- out %>% dplyr::mutate(prodno = as.integer(prodno))
  }

  return(out)

}

#' Pull California county SpatialPolygonsDataFrame.
#'
#' \code{pull_spdf} pulls either the section or township-level
#' SpatialPolygonsDataFrame from a county's Geographic Information System (GIS)
#' shapefile.
#'
#' @inheritParams find_counties
#' @param section_township Either "section" (the default) or "township".
#'   Specifies whether you would like to pull a section- or township-level
#'   SpatialPolygonsDataFrame.
#' @param download_progress TRUE / FALSE indicating whether you would like a
#'   message and progress bar printed for the shapefile that is downloaded.
#'   The default value is FALSE
#'
#' @return A SpatialPolygonsDataFrame object.
#'
#' @section Source:
#' SpatialPolygonDataFrame objects are downloaded from GIS shapefiles provided
#' by the California Department of Pesticide Regulation:
#' \url{http://www.cdpr.ca.gov/docs/emon/grndwtr/gis_shapefiles.htm}
#'
#' @section Note:
#' If this function returns an error (because the FTP site is down, for
#' example), check your working directory. You may want to change it back from
#' a temporary directory.
#'
#' @examples
#' \dontrun{
#' trinity_shp <- pull_spdf("trinity", download_progress = TRUE)
#' plot(trinity_shp)
#'
#' del_norte_shp <- pull_spdf("08", "township", download_progress = TRUE)
#' plot(del_norte_shp)
#' }
#' @export
pull_spdf <- function(county, section_township = "section",
                      download_progress = FALSE) {

  if (county != "Statewide") {
    county_name <- find_counties(county, return = "names")
    county_name <- stringr::str_replace(county_name, " ", "_")

    shp_url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/grndwtr/",
                      county_name, "/", county_name, "_", section_township,
                      "s.zip")
    file <- paste0(county_name, "_", section_township, "s.zip")
  } else {
    shp_url <- "ftp://transfer.cdpr.ca.gov/pub/outgoing/grndwtr/Statewide/mtrnet.zip"
    file <- "mtrnet.zip"
  }

  current_dir <- getwd()
  temp_dir <- tempdir()
  setwd(temp_dir)

  invisible(suppressMessages(suppressWarnings(file.remove(list.files(temp_dir)))))

  if (download_progress) {
    quiet <- FALSE
  } else {
    quiet <- TRUE
  }

  download.file(shp_url, destfile = file, mode = "wb", quiet = quiet)

  unzip(file, exdir = temp_dir)

  shp_file <- list.files()[grepl(".shp", list.files()) &
                             !grepl(".xml", list.files())]

  shp <- rgdal::readOGR(shp_file,
                        layer = basename(strsplit(shp_file, "\\.")[[1]])[1],
                        verbose = FALSE)
  shp <- sp::spTransform(shp, sp::CRS("+init=epsg:4326"))

  setwd(current_dir)

  return(shp)

}

#' Pull PUR Product Table.
#'
#' This function pulls a California Department of Pesticide Regulation Product
#' Table for a particular year.
#'
#' @param year A four digit year in the range of 1990 to 2015.
#' @param download_progress TRUE / FALSE indicating whether you would like a
#'   message and progress bar printed for the product table that is downloaded.
#'   The default value is FALSE.
#'
#' @return A data frame with four columns:
#' \describe{
#' \item{prodno}{Integer. The California Registration number for the pesticide
#' product. This corresponds to the \code{prodno} column in a raw or cleaned PUR
#' dataset returned from \code{pull_raw_pur} or \code{pull_clean_pur}.}
#' \item{prodstat_ind}{Character. An indication of product registration status:
#'   \itemize{
#'   \item A = Active
#'   \item B = Inactive
#'   \item C = Inactive, Not Renewed
#'   \item D = Inactive, Voluntary Cancellation
#'   \item E = Inactive, Cancellation
#'   \item F = Inactive, Suspended
#'   \item G = Inactive, Invalid Data
#'   \item H = Active, Suspended}}
#' \item{product_name}{Character. The name of the product taken from the
#' registered product label. May have been modified by DPR's Registration Branch
#' to ensure uniqueness.}
#' \item{signlwrd_ind}{Integer. The signal word printed on the front of the
#' product label:
#'   \itemize{
#'   \item 1 = Danger (Poison)
#'   \item 2 = Danger (Only)
#'   \item 3 = Warning
#'   \item 4 = Caution
#'   \item 5 = None}}
#' \item{year}{Integer. Four digit year indicating the year for which data was
#' pulled.}
#' }
#'
#' @examples
#' \dontrun{
#' prod_95 <- pull_product_table(1995)
#' }
#' @importFrom magrittr %>%
pull_product_table <- function(year, download_progress = FALSE) {

  url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
                year, ".zip")
  file <- paste0("pur", year, ".zip")
  current_dir <- getwd()
  dir <- tempdir()

  if (download_progress) {
    quiet <- FALSE
  } else {
    quiet <- TRUE
  }

  setwd(dir)
  download.file(url, destfile = file, mode = "wb", quiet = quiet)
  unzip(file, exdir = dir)

  suppressWarnings(suppressMessages(
    product_file <- readr::read_csv("product.txt") %>%
      dplyr::select(prodno, prodstat_ind, product_name, signlwrd_ind) %>%
      dplyr::mutate(prodno = as.integer(prodno),
                    prodstat_ind = as.character(prodstat_ind),
                    product_name = as.character(product_name),
                    signlwrd_ind = as.integer(signlwrd_ind),
                    year = as.integer(year))
  ))

  setwd(current_dir)

  return(product_file)

}
