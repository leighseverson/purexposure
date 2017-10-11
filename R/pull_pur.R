#' Search for active ingredients applied in a year.
#'
#' \code{chemical_search} returns a list of data frames with active ingredients
#' (\code{chemname}) matching each search term given in the \code{chemicals}
#' argument for a particular year.
#'
#' @param year A four-digit  year in the range of 1990 to 2015. Indicates
#'   the year in which you would like to search for active ingredients.
#' @param chemicals A string or vector of strings giving search terms of
#'   chemicals to match with active ingredients present in pesticides applied
#'   in the given year.
#'
#' @return A list of data frames (one data frame per search term). Each data
#'   frame has two columns: \code{chemname}, giving the active ingredient
#'   present in PUR records for that year, and \code{search_term}, giving the
#'   corresponding search term provided in the \code{chemicals} argument.
#'   List elements are named according to given search terms.
#'
#' @section Note:
#' The PUR Chemical Lookup Table for a year lists all active ingredients present
#' in applied pesticides across the state of California. Therefore, PUR data for
#' a particular county may not include records for active ingredients returned
#' by \code{chemical_search} for the same year.
#'
#' @examples
#' \dontrun{
#' chemical_search(2000, "methyl bromide")
#' chemical_search(1995, c("ammonia", "benzene"))
#' }
#' @importFrom dplyr %>%
#' @export
chemical_search <- function(year, chemicals) {

  df <- purexposure::chemical_list
  df <- df[[as.character(year)]]

  chems_up <- toupper(chemicals)

  pull_chemical_name <- function(chemical) {

    chem_up <- toupper(chemical)
    df2 <- df[grep(chem_up, df$chemname), ]
    df2 <- df2 %>% dplyr::mutate(search_term = chemical,
                                 chemname = as.factor(chemname)) %>%
      dplyr::select(chemname, search_term)

    return(df2)

  }

  out_list <- purrr::map(chemicals, pull_chemical_name)
  names(out_list) <- chemicals

  return(out_list)

}

#' Pull active ingredient chemical codes from PUR Chemical Lookup Tables.
#'
#' For a vector of chemical names, \code{chemical_codes} returns
#' a data frame with corresponding chemical codes from the PUR Chemical Lookup
#' Table for a given year.
#'
#' @param year A four-digit numeric year in the range of 1990 to 2015. Indicates
#'   the year in which you would like to match chemical codes.
#' @param chemicals A string or vector of strings giving search terms of
#'   chemicals to match with active ingredients present in pesticides applied
#'   in the given year. The default value is "all", which returns codes for all
#'   active ingredients applied in a given year.
#'
#' @return A data frame with three columns:
#'   \itemize{
#'     \item \code{chem_code}, with chemical codes corresponding to each active
#'     ingredient. \code{chem_code} values are used to later filter raw PUR
#'     datasets.
#'     \item \code{chemname}, with unique active ingredients corresponding to
#'     each search term.
#'     \item \code{chemical}, with search terms given in the \code{chemicals}
#'     argument.
#'     }
#'
#' @section Note:
#' The PUR Chemical Lookup Table for a year lists all active ingredients present
#' in applied pesticides across the state of California. Therefore, PUR data for
#' a particular county may not include records for active ingredients returned
#' by \code{chemical_search} for the same year.
#'
#' @examples
#' \dontrun{
#' chemical_codes(2000, "methyl bromide")
#' chemical_codes(1995, c("ammonia", "benzene"))
#' }
#' @importFrom dplyr %>%
#' @export
chemical_codes <- function(year, chemicals = "all") {

  df <- purexposure::chemical_list
  df <- df[[as.character(year)]]

  pull_chemical_code <- function(chemical) {

    chem_up <- toupper(chemical)
    if (chem_up == "ALL") {
      df2 <- df
    } else {
      df2 <- df[grep(chem_up, df$chemname), ]
      df2 <- df2 %>% dplyr::mutate(chemical = chemical) %>%
        dplyr::select(-chemalpha_cd)
    }

    return(df2)

  }

  out <- purrr::map_dfr(chemicals, pull_chemical_code)

  return(out)

}

#' Find California county codes
#'
#' Given a county, \code{find_county_code} returns a PUR county code.
#'
#' @inheritParams pull_pur_file
#'
#' @return A two-character string giving the corresponding PUR county code.
#'
#' @examples
#' \dontrun{
#' find_county_code("el dorado")
#' find_county_code("45")
#' }
#' @export
find_county_code <- function(county) {

  code_df <- purexposure::county_codes

  test <- suppressWarnings(as.numeric(county))
  if (is.na(test)) {
    county_upper <- toupper(county)
    county_nm <- grep(county_upper, code_df$county_name, value = TRUE)
    code <- as.character(code_df %>%
                           dplyr::filter(county_name == county_nm) %>%
                           dplyr::select(county_code))
  } else {
    county_cd <- county
    code <- grep(county_cd, code_df$county_code, value = TRUE)
  }

  return(code)

}

#' Pull raw PUR file for a year and county or counties.
#'
#' \code{pull_pur_file} pulls the raw PUR dataset for a particular year.
#'
#' @inheritParams pull_raw_pur
#' @param county A character string giving either a county name or a two digit
#'  county code. Not case sensitive. California names and county codes as they
#'  appear in PUR dataset can be found in the county_codes dataset available
#'  with this package. For example, to return data for Alameda county, enter
#'  either "alameda" or "01" for the county argument.
#'
#' @return A data frame with 33 columns. Year for which data was pulled is
#'   indicated by \code{applic_dt}; county is indicated by \code{county_cd}.
#'
#' \dontrun{
#' raw_file <- pull_pur_file(1999, "ventura")
#' }
#' @export
pull_pur_file <- function(year, county, download_progress = FALSE) {

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

  code <- find_county_code(county)

  raw_data <- suppressWarnings(suppressMessages(
    readr::read_csv(paste0("udc", sm_year, "_", code, ".txt"))
  ))

  raw_data <- dplyr::mutate_all(raw_data, as.character)

  setwd(current_dir)
  return(raw_data)

}

#' Pull raw PUR data by counties and years
#'
#' \code{pull_raw_pur} pulls a raw PUR dataset for a given California county and
#' year.
#'
#' @param years A four-digit numeric year or vector of years in the range of
#'   1990 to 2015. Indicates the years for which you would like to pull PUR
#'   datasets. \code{years == "all"} will pull data from 1990 through 2015.
#' @param counties A character string or vector of character strings giving either
#'   county names or a two digit county code for each county. Not case sensitive.
#'   California names and county codes as they appear in PUR dataset can be found
#'   in the \code{county_codes} dataset available with this package. For example,
#'   to return data for Alameda county, enter either "alameda" or "01" for the
#'   \code{counties} argument.
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
#' df <- pull_raw_pur(years = 1990:1993, counties = c("01", "02", "10"))
#' df2 <- pull_raw_pur(years = c(2000, 2010), counties = c("butte", "15", "01"))
#' df3 <- pull_raw_pur(years = 2015, counties = c("colusa"))
#' }
#' @importFrom dplyr %>%
#' @export
pull_raw_pur <- function(years, counties, verbose = TRUE, download_progress = FALSE) {

  if ("all" %in% tolower(years)) {
    years <- 1990:2015
  }

  code_df <- purexposure::county_codes

  ## error handling

  if (!all(is.character(counties))) {
    stop("County names and/or codes should be character strings.")
  }
  if (!all(is.numeric(years))) {
    stop("Years should be four-digit numeric values.")
  }
  if (all(is.numeric(years)) & (min(years) < 1990 | max(years) > 2015)) {
    stop("Years should be between 1990 and 2015.")
  }

  test <- suppressWarnings(as.numeric(counties))
  order <- data.frame(counties = counties, order = 1:length(counties))

  names <- counties[grep(TRUE, is.na(test))]
  if (length(names) != 0) {
    for (k in 1:length(names)) {

      county_nm <- toupper(names[k])
      county_test <- grep(county_nm, code_df$county_name, value = TRUE)

      if (length(county_test) != 1) {
        stop(writeLines(paste0("\"", names[k], "\"", " doesn't match any ",
                               "California counties. \nCheck out the ",
                               "county_codes data set included with this ",
                               "package for county names and corresponding ",
                               "codes.")))
      }

      s <- strsplit(names[k], " ")[[1]]
      county_name <- paste(toupper(substring(s, 1,1)), substring(s, 2),
                           sep = "", collapse = " ")
      order_df <- data.frame(counties = names[k], name_clean = county_name)
      if (k == 1) {
        county_name_out <- county_name
        order_df_out <- order_df
      } else {
        county_name_out <- c(county_name_out, county_name)
        order_df_out <- rbind(order_df_out, order_df)
      }
    }
  }

  codes <- counties[grep(FALSE, is.na(test))]
  if (length(codes) != 0) {
    for (l in 1:length(codes)) {

      county_cd <- codes[l]

      county_code <- grep(county_cd, code_df$county_code, value = TRUE)

      if (length(county_code) != 1) {
        stop(writeLines(paste0("\"", codes[l], "\"", " doesn't match any ",
                               "California counties.\nCheck out the ",
                               "county_codes dataset included with this ",
                               "package for county names and corresponding ",
                               "codes.")))
      }

      county_name2 <- as.character(code_df %>%
        dplyr::filter(county_code == codes[l]) %>%
        dplyr::select(county_name))
      county_name2 <- tolower(county_name2)
      s2 <- strsplit(county_name2, " ")[[1]]
      county_name2 <- paste(toupper(substring(s2, 1,1)), substring(s2, 2),
                            sep = "", collapse = " ")
      order_df2 <- data.frame(counties = codes[l], name_clean = county_name2)
      if (l == 1) {
        county_name_out2 <- county_name2
        order_df_out2 <- order_df2
      } else {
        county_name_out2 <- c(county_name_out2, county_name2)
        order_df_out2 <- rbind(order_df_out2, order_df2)
      }
    }
  }

  if (length(names) != 0 & length(codes) != 0) {
    order_df <- rbind(order_df_out, order_df_out2)
  } else if (length(names) != 0 & length(codes) == 0) {
    order_df <- order_df_out
  } else if (length(names) == 0 & length(codes) != 0) {
    order_df <- order_df_out2
  }

  order_df_full <- suppressWarnings(dplyr::full_join(order_df, order,
                                                     by = "counties") %>%
                                      dplyr::arrange(order))

  names_clean <- as.character(order_df_full$name_clean)

  ## messaging

  if (verbose) {

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

    message(paste0("Pulling PUR data for ", county_message, " for ",
                   year_message, " Great choice!"))

  }

  ## pull data

  years_counties <- expand.grid(year = years, county = counties)
  years_counties <- dplyr::mutate(years_counties, county = as.character(county))
  raw_df_all <- purrr::map2_dfr(years_counties$year, years_counties$county,
                                pull_pur_file)

  return(raw_df_all)

}

#' Pull cleaned PUR data by counties, years, and chemicals
#'
#' \code{pur_data} returns a data frame of cleaned Pesticide Use Report data
#' filtered by counties, years, and active ingredients. Active ingredients
#' present in applied pesticides can be summed by either Public Land Survey
#' (PLS) section or township.
#'
#' @inheritParams pull_raw_pur
#' @inheritParams chemical_codes
#' @param sum_application TRUE / FALSE indicating if you would like to sum the
#'   amounts of applied active ingredients by day and by the geographic unit
#'   given in \code{unit}. The default value is TRUE.
#' @param unit A character string giving either "section" or "township".
#'   Specifies whether applications of each active ingredient should be summed
#'   by California section (the default) or by township if \code{sum_application} is
#'   \code{TRUE}.
#' @param include_ag TRUE / FALSE indicating if you would like to retain
#'   aerial/ground application data when summing application across sections or
#'   townships. The default is TRUE. \code{include_ag = TRUE} could result in
#'   more than one record per section, date, and active ingredient if
#'   \code{aerial_ground} application differs for the same chemical in the same
#'   section and on the same day.
#'
#' @return A data frame with 11 columns:
#'   \itemize{
#'     \item \code{chem_code} An integer value giving the PUR chemical code
#'     for the active ingredient applied.
#'     \item \code{chemname} A character string giving PUR chemical active
#'     ingredient names. Unique values of \code{chemname} are matched with terms
#'     provided in the \code{chemicals} argument.
#'     \item \code{kg_chm_used} A numeric value giving the amount of the active
#'     ingredient applied (kilograms).
#'     \item \code{section} A string nine characters long indicating the section
#'     of application. PLS sections are uniquely identified by a combination of
#'     base line meridian (S, M, or H), township (01-48), township direction
#'     (N or S), range (01-47), range direction (E or W) and section number
#'     (01-36). (This column is not included if \code{sum_application} = TRUE).
#'     \item \code{township} A string 7 characters long indicating the township
#'     of application. PLS townships are uniquely identified by a combination of
#'     base line meridian (S, M, or H), township (01-48), township direction
#'     (N or S), range (01-47), and range direction (E or W).
#'     \item \code{county_name} A character string giving the county name where
#'     application took place.
#'     \item \code{county_code} A string two characters long giving the PUR county
#'     code where application took place.
#'     \item \code{date} the date of application (yyyy-mm-dd).
#'     \item \code{aerial_ground} A character giving the application method.
#'     "A" = aerial, "G" = ground, and "O" = other. (This column is not included if
#'     you \code{sum_application} = TRUE and \code{include_ag} = FALSE.)
#'     \item \code{use_no} A character ID ID identifing unique application of an
#'     active ingredient across years. This value is a combination of the raw PUR
#'     \code{use_no} and the year of application.
#'     \item \code{outlier} A logical value indicating whether the
#'     amount listed in \code{lbs_chm_used} has been corrected large amounts
#'     entered in error. The algorithm for identifying and replacing outliers
#'     was developed based on methods used by Gunier et al. (2001). Please see
#'     the package vignette for more detail regarding these methods. (This column
#'     is not included if \code{sum_application} = TRUE).
#'   }
#'
#' @section Note: For documentation of raw PUR data, see the Pesticide Use
#'   Report Data User Guide & Documentation document published by the California
#'   Department of Pesticide Regulation. This file is saved as "cd_doc.pdf" in any
#'   "pur[year].zip" file between 1990 and 2015 found here:
#'   \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/}.
#'
#' @examples
#' \dontrun{
#'
#' }
#' @importFrom dplyr %>%
#' @export
pur_data <- function(years, counties, chemicals, sum_application = TRUE,
                     unit = "section", include_ag = TRUE, verbose = TRUE,
                     download_progress = FALSE) {

  raw_df <- pull_raw_pur(years = years, counties = counties, verbose = verbose,
                         download_progress = download_progress)

  years_chemicals <- expand.grid(year = years, chemicals = chemicals)
  years_chemicals <- dplyr::mutate(years_chemicals,
                                   chemicals = as.character(chemicals))
  chem_df <- purrr::map2_dfr(years_chemicals$year, years_chemicals$chemical,
                             chemical_codes)
  chem_df <- chem_df %>%
    dplyr::mutate(chemical = factor(chemical, levels = chemicals)) %>%
    dplyr::arrange(chemical) %>%
    unique()

  df <- raw_df %>%
    dplyr::mutate(township_pad = stringr::str_pad(raw_df$township, 2, "left", pad = "0"),
                  range = stringr::str_pad(raw_df$range, 2, "left", pad = "0"),
                  section = stringr::str_pad(raw_df$section, 2, "left", pad = "0"),
                  MTRS = as.character(paste0(base_ln_mer, township_pad, tship_dir,
                                             range, range_dir, section)),
                  township = as.character(paste0(base_ln_mer, township_pad, tship_dir)),
                  MTR = as.character(paste0(township, range, range_dir))) %>%
    dplyr::select(chem_code, lbs_chm_used, MTRS, MTR, county_cd, applic_dt,
                  aer_gnd_ind, use_no, acre_treated, unit_treated) %>%
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
    dplyr::group_by(chem_code) %>%
    dplyr::summarize(mean = mean(lbs_per_acre, na.rm = TRUE),
                     sd = sd(lbs_per_acre, na.rm = TRUE)) %>%
    dplyr::mutate(calc_max = mean + 2*sd)
  df <- df %>%
    dplyr::filter(chem_code %in% chem_df$chem_code) %>%
    dplyr::left_join(chem_df, by = "chem_code")
  df2 <- calc_max %>%
    dplyr::select(chem_code, calc_max) %>%
    dplyr::right_join(df, by = "chem_code") %>%
    dplyr::mutate(outlier = ifelse(lbs_per_acre > calc_max, TRUE, FALSE),
                  lbs_chm_used = ifelse(lbs_per_acre > calc_max,
                                        calc_max*acre_treated, lbs_chm_used)) %>%
    dplyr::mutate(applic_dt = lubridate::mdy(applic_dt)) %>%
    dplyr::rename(county_code = county_cd)

  county <- purexposure::county_codes

  df3 <- county %>%
    dplyr::right_join(df2, by = "county_code") %>%
    dplyr::mutate(use_no = paste0(use_no, "_", lubridate::year(applic_dt)),
                  kg_chm_used = lbs_chm_used/2.20562) %>%
    dplyr::select(chem_code, chemname, kg_chm_used, MTRS, MTR, county_name,
                  county_code, applic_dt, aer_gnd_ind, use_no, outlier) %>%
    dplyr::rename(section = MTRS,
                  township = MTR,
                  date = applic_dt,
                  aerial_ground = aer_gnd_ind) %>%
    dplyr::arrange(date)

  if (sum_application) {

    if (unit == "section") {
      df3 <- df3 %>% dplyr::group_by(section, chem_code, chemname, county_code,
                                     county_name, date, aerial_ground, use_no) %>%
        dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(chem_code, chemname, kg_chm_used, section, county_name,
                      county_code, date, aerial_ground, use_no)
    } else if (unit == "township") {
      df3 <- df3 %>% dplyr::group_by(township, chem_code, chemname, county_code,
                                     county_name, date, aerial_ground, use_no) %>%
        dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(chem_code, chemname, kg_chm_used, township, county_name,
                      county_code, date, aerial_ground, use_no)
    }

  }





}



















