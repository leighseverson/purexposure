#' Pull raw PUR data by counties and years.
#'
#' \code{pull_raw_pur} pulls a raw PUR data set for a given year and vector of
#'   California counties.
#'
#' PUR data sets are pulled by county from the CDPR's FTP server. Downloaded
#' PUR data sets are saved in a temporary environment, which is deleted at the
#' end of the current R session.
#'
#' @param years A four-digit numeric year or vector of years in the range of
#'   1990 to 2015. Indicates the years for which you would like to pull PUR
#'   data sets. \code{years == "all"} will pull data from 1990 through 2015.
#' @param counties A vector of character strings giving either a county name,
#'   two digit PUR county codes, or six-digit FIPS county codes for each county.
#'   Not case sensitive. California names, county codes as they appear in PUR
#'   data sets, and FIPS county codes can be found in the \code{county_codes}
#'   data set available with this package. For example, to return data for
#'   Alameda county, enter either "alameda", "01", or "06001" for the
#'   \code{counties} argument. \code{counties = "all"} will return data for all
#'   58 California counties (this will take a while to run).
#' @param verbose TRUE / FALSE indicating whether you would like a single message
#'   printed indicating which counties and years you are pulling data for. The
#'   default value is TRUE.
#' @param quiet TRUE / FALSE indicating whether you would like a
#'   message and progress bar printed for each year of PUR data that
#'   is downloaded. The default value is FALSE.
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
#'   example), check your working directory. You may need to change it back from
#'   a temporary directory.}
#' }
#'
#' @examples
#' \donttest{
#' df <- pull_raw_pur(years = 2000, counties = "fresno")
#' df2 <- pull_raw_pur(years = c(2000, 2010), counties = c("butte", "15", "06001"))
#' df3 <- pull_raw_pur(years = 2015, counties = c("colusa"))
#' }
#' @importFrom magrittr %>%
#' @export
pull_raw_pur <- function(years = "all", counties = "all", verbose = TRUE,
                         quiet = FALSE) {

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

    raw_df <- purrr::map_dfr(years, help_pull_pur, counties = counties,
                             quiet = quiet)

  } else {

    raw_df <- purrr::map_dfr(years, help_pull_pur, counties = "all",
                             quiet = quiet)

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
#' PUR data sets are pulled by county from the CDPR's FTP server. Downloaded
#' PUR data sets are saved in a temporary environment, which is deleted at the
#' end of the current R session.
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
#'   \code{chemical_list} data set included with this package). The
#'   \code{chemical_class} column should have character strings indicating the
#'   chemical class corresponding to each \code{chem_code}. The
#'   \code{chemical_class} for a group of active ingredients should be decided
#'   upon by the user. Only used if \code{sum = "chemical_class"}. See
#'   the CDPR's Summary of PUR Data document here:
#'   \url{http://www.cdpr.ca.gov/docs/pur/pur08rep/chmrpt08.pdf} for
#'   comprehensive classifications of active ingredients.
#' @param aerial_ground TRUE / FALSE indicating if you would like to
#'   retain aerial/ground application data ("A" = aerial, "G" = ground, and
#'   "O" = other.) The default is TRUE.
#' @param ... Used internally.
#'
#' @return A data frame:
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
#'     \item{township}{A string seven characters long indicating the township
#'     of application. PLS townships are uniquely identified by a combination of
#'     base line meridian (S, M, or H), township (01-48), township direction
#'     (N or S), range (01-47), and range direction (E or W).}
#'     \item{county_name}{A character string giving the county name where
#'     application took place.}
#'     \item{pur_code}{A string two characters long giving the PUR county
#'     code where application took place.}
#'     \item{fips_code}{A string six characters long giving the FIPS county
#'     code where application took place.}
#'     \item{date}{The date of application (yyyy-mm-dd).}
#'     \item{aerial_ground}{A character giving the application method.
#'     "A" = aerial, "G" = ground, and "O" = other. Not included
#'     if \code{aerial_ground = FALSE}.}
#'     \item{use_no}{A character string identifying unique application of an
#'     active ingredient across years. This value is a combination of the raw PUR
#'     \code{use_no} column and the year of application. Not included if
#'     \code{sum_appliction = TRUE}.}
#'     \item{outlier}{If the amount listed in \code{kg_chm_used} has been
#'     corrected for large amounts entered in error, this column lists the raw
#'     value of recorded kilograms of applied chemicals. Otherwise \code{NA}. The
#'     algorithm for identifying and replacing outliers was developed based on
#'     methods used by Gunier et al. (2001). Please see the package vignette for
#'     more detail regarding these methods. Not included if
#'     \code{sum_application = TRUE}.}
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
#'     records for active ingredients listed in the \code{chemical_list} data set
#'     for the same year.
#'     \item To pull raw PUR data, see the \code{pull_raw_pur} function.
#'     For documentation of raw PUR data, see the Pesticide Use Report Data User
#'     Guide & Documentation document published by the California Department of
#'     Pesticide Regulation. This file is saved as "cd_doc.pdf" in
#'     any "pur[year].zip" file between 1990 and 2015 found here:
#'     \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/}.
#'     \item{If this function returns an error (because the FTP site is down,
#'     for example), check your working directory. You may need to change it
#'     back from a temporary directory.}
#' }
#'
#' @examples
#' library(magrittr)
#' \dontshow{
#' fresno_raw <- readRDS(system.file("extdata", "fresno_raw.rds", package = "purexposure"))
#' df <- pull_clean_pur(2000, "fresno", raw_pur = fresno_raw)}
#' \donttest{
#' df <- pull_clean_pur(years = 2000:2001,
#'                      counties = c("06001", "29", "riverside"),
#'                      chemicals = "methylene",
#'                      aerial_ground = TRUE)
#'
#' # filter to active ingredients present in particular products
#' prod_nos <- find_product_name(2003, "insecticide") %>%
#'     dplyr::select(prodno) %>%
#'     tibble_to_vector()
#'
#' df2 <- pull_clean_pur(2003, "10") %>%
#'     dplyr::filter(prodno %in% prod_nos)
#'
#' # Sum application by active ingredients
#' df3 <- pull_clean_pur(years = 2009:2010,
#'                       counties = c("01", "29", "riverside"),
#'                       unit = "township",
#'                       sum_application = TRUE)
#'
#' # Or by chemical classes
#' chemical_class_df <- rbind(find_chemical_codes(2000, "methylene"),
#'                            find_chemical_codes(2000, "aldehyde")) %>%
#'    dplyr::rename(chemical_class = chemical)
#'
#' df4 <- pull_clean_pur(years = 1995,
#'                       counties = "fresno",
#'                       chemicals = chemical_class_df$chemname,
#'                       sum_application = TRUE,
#'                       sum = "chemical_class",
#'                       unit = "township",
#'                       chemical_class = chemical_class_df)
#' }
#' @importFrom magrittr %>%
#' @export
pull_clean_pur <- function(years = "all", counties = "all", chemicals = "all",
                           sum_application = FALSE, unit = "section",
                           sum = "all", chemical_class = NULL,
                           aerial_ground = TRUE, verbose = TRUE,
                           quiet = FALSE, ...) {

  args <- list(...)
  if (is.null(args$raw_pur)) {
    raw_df <- pull_raw_pur(years = years, counties = counties, verbose = verbose,
                           quiet = quiet)
  } else {
    raw_df <- args$raw_pur
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
                     sd = stats::sd(lbs_per_acre, na.rm = TRUE)) %>%
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
                                      lbs_per_acre > calc_max), lbs_chm_used, NA),
                  lbs_chm_used = ifelse(lbs_per_acre > calc_max,
                                        calc_max*acre_treated, lbs_chm_used)) %>%
    dplyr::rename(pur_code = county_cd) %>%
    dplyr::ungroup()

  county <- purexposure::county_codes

  out <- county %>%
    dplyr::right_join(df2, by = "pur_code") %>%
    dplyr::mutate(use_no = paste0(use_no, "_", lubridate::year(applic_dt)),
                  kg_chm_used = lbs_chm_used/2.20562) %>%
    dplyr::select(chem_code, chemname, kg_chm_used, MTRS, MTR, county_name,
                  pur_code, fips_code, applic_dt, aer_gnd_ind, use_no, outlier,
                  prodno) %>%
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
                                                   NA, township))
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
                                    chemname, section, county_name, pur_code,
                                    fips_code, date, aerial_ground)
        } else {
          out <- help_sum_application(out, "all", "section", FALSE,
                                    section_townships,
                                    chem_code,
                                    chemname, section, county_name, pur_code,
                                    fips_code, date)
        }
      } else if (unit == "township") {
        if (aerial_ground) {
          out <- help_sum_application(out, "all", "township", TRUE,
                                    section_townships,
                                    chem_code,
                                    chemname, township, county_name, pur_code,
                                    fips_code, date, aerial_ground)
        } else {
          out <- help_sum_application(out, "all", "township", FALSE,
                                    section_townships,
                                    chem_code,
                                    chemname, township, county_name, pur_code,
                                    fips_code, date)
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
                                    pur_code, fips_code, date, aerial_ground)
        } else {
          out <- help_sum_application(out, "chemical_class", "section", FALSE,
                                    section_townships,
                                    chemical_class = chemical_class,
                                    chemical_class, section, county_name,
                                    pur_code, fips_code, date)
        }
      } else if (unit == "township") {
        if (aerial_ground) {
          out <- help_sum_application(out, "chemical_class", "township", TRUE,
                                    section_townships,
                                    chemical_class = chemical_class,
                                    chemical_class, township, county_name,
                                    pur_code, fips_code, date, aerial_ground)
        } else {
          out <- help_sum_application(out, "chemical_class", "township", FALSE,
                                    section_townships,
                                    chemical_class = chemical_class,
                                    chemical_class, township, county_name,
                                    pur_code, fips_code, date)
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
#' SpatialPolygonsDataFrame objects are pulled by county from the CDPR's FTP
#' server. Downloaded SpatialPolygonsDataFrame objects are saved in a temporary
#' environment, which is deleted at the end of the current R session.
#'
#' @param county A character string giving either a county name, two digit PUR
#'  county code, or six-digit FIPS county code. Not case sensitive. California
#'  names and county codes as they appear in PUR data sets can be found in the
#'  \code{county_codes} data set available with this package.
#' @param section_township Either "section" (the default) or "township".
#'   Specifies whether you would like to pull a section- or township-level
#'   SpatialPolygonsDataFrame.
#' @param quiet TRUE / FALSE indicating whether you would like a
#'   message and progress bar printed for the shapefile that is downloaded.
#'   The default value is FALSE.
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
#' \donttest{
#' fresno_shp <- pull_spdf("fresno")
#' fresno_shp %>% spdf_to_df() %>% df_plot()
#' del_norte_shp <- pull_spdf("08", "township")
#' del_norte_shp %>% spdf_to_df() %>% df_plot()
#' }
#' @export
pull_spdf <- function(county, section_township = "section",
                      quiet = FALSE) {

  if (county != "Statewide") {

    county_name <- find_counties(county, return = "names")
    county_name_underscore <- stringr::str_replace(county_name, " ", "_")

    if (county_name == "Los Angeles") {
      shp_url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/grndwtr/",
                        "Los_Angeles/LosAngeles_", section_township, "s.zip")
      file <- paste0("LosAngeles_", section_township, "s.zip")
    } else if (county_name == "San Luis Obispo") {
      shp_url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/grndwtr/",
                        county_name_underscore, "/SLO_", section_township, "s.zip")
      file <- paste0("SLO_", section_township, "s.zip")
    } else {
      shp_url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/grndwtr/",
                        county_name_underscore, "/", county_name_underscore, "_",
                        section_township, "s.zip")
      file <- paste0(county_name_underscore, "_", section_township, "s.zip")
    }

  } else {
    shp_url <- "ftp://transfer.cdpr.ca.gov/pub/outgoing/grndwtr/Statewide/mtrnet.zip"
    file <- "mtrnet.zip"
  }

  current_dir <- getwd()

  if (!exists("purexposure_package_env")) {

    temp_dir <- tempdir()
    setwd(temp_dir)

    purexposure_package_env <<- new.env()
    purexposure_package_env$pur_lst <- list()

    invisible(suppressMessages(suppressWarnings(file.remove(list.files(temp_dir)))))
    utils::download.file(shp_url, destfile = file, mode = "wb", quiet = quiet)
    utils::unzip(file, exdir = temp_dir)

    shp_file <- list.files()[grepl(".shp", list.files()) &
                               !grepl(".xml", list.files())]
    shp <- rgdal::readOGR(shp_file,
                          layer = basename(strsplit(shp_file, "\\.")[[1]])[1],
                          verbose = FALSE)
    shp <- sp::spTransform(shp, sp::CRS("+init=epsg:4326"))

    suppressWarnings(suppressMessages(
      purexposure_package_env$pur_lst[[paste0(county_name_underscore,
                                              "_", section_township)]] <- shp
    ))

  } else {

    to_be_downloaded <- c()
    if (is.null(purexposure_package_env$pur_lst[[paste0(county_name_underscore,
                                                        "_", section_township)]])) {
      to_be_downloaded <- county_name_underscore
    }

    if(!is.null(to_be_downloaded)) {

      temp_dir <- tempdir()
      setwd(temp_dir)

      invisible(suppressMessages(suppressWarnings(file.remove(list.files(temp_dir)))))
      utils::download.file(shp_url, destfile = file, mode = "wb", quiet = quiet)
      utils::unzip(file, exdir = temp_dir)

      shp_file <- list.files()[grepl(".shp", list.files()) &
                                 !grepl(".xml", list.files())]
      shp <- rgdal::readOGR(shp_file,
                            layer = basename(strsplit(shp_file, "\\.")[[1]])[1],
                            verbose = FALSE)
      shp <- sp::spTransform(shp, sp::CRS("+init=epsg:4326"))

      suppressWarnings(suppressMessages(
        purexposure_package_env$pur_lst[[paste0(county_name_underscore,
                                                "_", section_township)]] <- shp
      ))

    }

  }

  shp <- purexposure_package_env$pur_lst[[paste0(county_name_underscore,
                                                 "_", section_township)]]

  setwd(current_dir)

  return(shp)

}

#' Pull PUR Product Table.
#'
#' This function pulls a California Department of Pesticide Regulation Product
#' Table for a vector of years.
#'
#' Product tables are pulled by year from the CDPR's FTP server. Downloaded
#' tables are saved in a temporary environment, which is deleted at the end of
#' the current R session.
#'
#' @param years A vector of four digit years in the range of 1990 to 2015.
#' @param quiet TRUE / FALSE indicating whether you would like a
#'   message and progress bar printed for the product table that is downloaded.
#'   The default value is FALSE.
#'
#' @return A data frame with four columns:
#' \describe{
#' \item{prodno}{Integer. The California Registration number for the pesticide
#' product. This corresponds to the \code{prodno} column in a raw or cleaned PUR
#' data set returned from \code{pull_raw_pur} or \code{pull_clean_pur}.}
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
#' \donttest{
#' prod_95 <- pull_product_table(1995)}
#' @importFrom magrittr %>%
#' @export
pull_product_table <- function(years, quiet = FALSE) {

  current_dir <- getwd()

  if (!exists("purexposure_package_env")) {

    dir <- tempdir()
    setwd(dir)

    purexposure_package_env <<- new.env()
    purexposure_package_env$pur_lst <- list()

    for (i in 1:length(years)) {

      url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
                    years[i], ".zip")
      file <- paste0("pur", years[i], ".zip")

      utils::download.file(url, destfile = file, mode = "wb", quiet = quiet)
      utils::unzip(file, exdir = dir)

      suppressWarnings(suppressMessages(
        purexposure_package_env$pur_lst[[as.character(years[i])]] <-
          readr::read_csv("product.txt") %>%
          dplyr::select(prodno, prodstat_ind, product_name, signlwrd_ind) %>%
          dplyr::mutate(prodno = as.integer(prodno),
                        prodstat_ind = as.character(prodstat_ind),
                        product_name = as.character(product_name),
                        signlwrd_ind = as.integer(signlwrd_ind),
                        year = as.integer(years[i]))
      ))
    }

  } else {

    to_be_downloaded <- c()

    for (i in 1:length(years)) {
      if (is.null(purexposure_package_env$pur_lst[[as.character(years[i])]])) {
        to_be_downloaded <- c(to_be_downloaded, years[i])
      }
    }

    if (!is.null(to_be_downloaded)) {

      dir <- tempdir()
      setwd(dir)

      for (i in 1:length(to_be_downloaded)) {

        url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
                      to_be_downloaded[i], ".zip")
        file <- paste0("pur", to_be_downloaded[i], ".zip")

        utils::download.file(url, destfile = file, mode = "wb", quiet = quiet)
        utils::unzip(file, exdir = dir)

        suppressWarnings(suppressMessages(
          purexposure_package_env$pur_lst[[as.character(to_be_downloaded[i])]] <-
            readr::read_csv("product.txt") %>%
            dplyr::select(prodno, prodstat_ind, product_name, signlwrd_ind) %>%
            dplyr::mutate(prodno = as.integer(prodno),
                          prodstat_ind = as.character(prodstat_ind),
                          product_name = as.character(product_name),
                          signlwrd_ind = as.integer(signlwrd_ind),
                          year = as.integer(to_be_downloaded[i]))
        ))
      }

    }

  }

  for (i in 1:length(years)) {

    product_file <- purexposure_package_env$pur_lst[[as.character(years[i])]]

    if (i == 1) {
      product_file_out <- product_file
    } else {
      product_file_out <- rbind(product_file_out, product_file)
    }
  }

  setwd(current_dir)

  return(product_file_out)

}
