#' Search for active ingredients applied in a year.
#'
#' \code{chemical_search} returns a list of data frames with active ingredients
#' (\code{chemname}) matching each search term given in the \code{chemicals}
#' argument for a particular year.
#'
#' @param year A four-digit numeric year in the range of 1990 to 2015. Indicates
#'   the year in which you would like to search for active ingredients.
#' @param chemicals A string or vector of strings giving search terms of
#'   chemicals to match with active ingredients present in pesticides applied
#'   in the given year.
#'
#' @return A list of data frames (one data frame per search term). Each data
#'   frame has two columns: \code{chemname}, giving the active ingredient
#'   present in PUR records for that year, and \code{search_term}, giving the
#'   corresponding search term provided in the \code{chemicals} argument.
#'   Elements of the list are named according to given search terms.
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

  df <- get("chemical_list")
  year <- as.character(year)
  df <- df[[year]]

  chems_up <- toupper(chemicals)

  out <- list()

  for (i in 1:length(chems_up)) {
    df2 <- df[grep(chems_up[i], df$chemname), ]
    df2 <- df2 %>% dplyr::mutate(search_term = chemicals[i],
                                 chemname = as.factor(chemname)) %>%
      dplyr::select(chemname, search_term)
    name <- chemicals[i]
    out[[name]] <- df2
  }

  return(out)

}

#' Pull chemical codes from PUR Chemical Lookup Tables.
#'
#' For a vector of chemical names, \code{chemical_codes} returns
#' a data frame with corresponding chemical codes from the PUR Chemical Lookup
#' Table for a given year.
#'
#' @param year A four-digit numeric year in the range of 1990 to 2015. Indicates
#'   the year in which you would like to match chemical codes.
#' @param chemical A string or vector of strings giving search terms of
#'   chemicals to match with active ingredients present in pesticides applied
#'   in the given year. The default value is "all", which returns codes for all
#'   active ingredients applied in a given year.
#'
#' @return A data frame with three columns:
#'   \itemize{
#'     \item \code{chemical}, with search terms given in the \code{chemicals}
#'     argument,
#'     \item \code{chemname}, with unique active ingredients corresponding to
#'     each search term, and
#'     \item \code{chem_code}, with chemical codes corresponding to each active
#'     ingredient. \code{chem_code}s are used to later filter raw PUR datasets.
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
#' chemical_search(1995, c("ammonia", "benzene"))
#' }
#' @importFrom dplyr %>%
#' @export
chemical_codes <- function(year, chemicals = "all") {

  df <- get("chemical_list")
  year <- as.character(year)
  df <- df[[year]]

  chems_up <- toupper(chemicals)

  if ("ALL" %in% chems_up) {
    out <- df
  } else {
    for (i in 1:length(chems_up)) {
      df2 <- df[grep(chems_up[i], df$chemname), ]
      df2 <- dplyr::mutate(df2, chemical = chems[i])
      if (i == 1) {
        out <- df2
      } else {
        out <- rbind(out, df2)
      }
    }
  }

  return(out)

}

#' Pull raw PUR data by county and year
#'
#' \code{raw_pur} pulls a raw PUR dataset for a given California county and
#' year.
#'
#' @param county A character string or vector of character strings giving either
#'   county names or a two digit county codes. California names and county codes
#'   as they appear in PUR dataset can be found in the \code{county_codes}
#'   dataset available with this package. For example, to return data for
#'   Alameda county, enter either "alameda" or "01" for the \code{county}
#'   argument.
#' @inheritParams chemical_search
#'
#' @return A data frame with
#'
#' @examples
#' \dontrun{
#'
#' }
#' @importFrom dplyr %>%
#' @export
raw_pur <- function(county, year) {

  code_df <- purexposure::county_codes

  test <- suppressWarnings(as.numeric(county))
  if (is.na(test)) {
    county_nm <- toupper(county)

    # make sure county is an exact match
    county_name <- grep(county_nm, code_df$couty_name, value = TRUE)

    if (length(county_name) != 1) {
      stop("This county doesn't match any California counties. Check out the
            county_codes data set included with this package for names and
            corresponding codes.")
    }

    code <- dplyr::filter(code_df, county_name == county_name)$county_cd

  } else {
    county_cd <- county

    county_code <- grep(county_cd, code_df$county_code, value = TRUE)

    if (length(county_code) != 1) {
      stop("This county doesn't match any California counties. Check out the
           county_codes data set included with this package for names and
           corresponding codes.")
    }

    code <- county_code

  }

  url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
                year, ".zip")
  file <- paste0("pur", year, ".zip")

  current_dir <- getwd()
  dir <- tempdir()
  setwd(dir)

  download.file(url, destfile = file, mode = "wb", quiet = quiet)
  unzip(file, exdir = dir)

  sm_year <- substr(3, 4, year)
  raw_data <- suppressMessages(readr::read_csv(paste0("udc", sm_year, "_",
                                                    county_code, ".txt")))

  setwd(current_dir)

  return(raw_data)

}

#' Pull PUR data by year and vector of chemicals.
#'
#' \code{pur_data} returns a data frame of Pesticide Use Report data filtered
#' by county, year, active ingredients, and summed by either section or
#' township.
#'
#' @inheritParams raw_pur
#' @param years A four-digit numeric year in the range of 1990 to 2015. Indicates
#'   the year in which you would like to match chemical codes.
#'   \code{years == "all"} will pull data from 1990 through 2015.
#' @inheritParams chemical_codes
#' @param sum TRUE / FALSE indicating if you would like to sum the amounts of
#'   applied active ingredients by day and by the geographic unit given in
#'   \code{unit}. The default value is TRUE.
#' @param unit A character string giving either "section" or "township".
#'   Specifies whether applications of each active ingredient should be summed
#'   by California section (the default, \code{MTRS}) or by township
#'   (\code{MTR}).
#'
#' @return A data frame with twelve columns:
#'   \itemize{
#'     \item \code{chem_code} and \code{chemname} correspond to chemicals given
#'     in the \code{chemicals} argument.
#'     \item \code{lbs_chm_used} gives the amount of pesticide, in pounds,
#'     applied for a given \code{date} and active ingredient.
#'     \item \code{MTRS} and \code{MTR} indicate the section and township where
#'     application of a given active ingredient on a given day took place,
#'     respectively,
#'     \item \code{county_name} gives the county of application,
#'     \item \code{date} the date of application, and
#'     \item \code{use_no} gives an ID identifing unique application of an
#'     active ingredient.
#'     \item The \code{outlier} column is a logical value indicating whether the
#'     amount listed in \code{lbs_chm_used} has been corrected large amounts
#'     entered in error. The algorithm for identifying and replacing outliers
#'     was developed based on methods used by Gunier et al. (2001). Please see
#'     the package vignette for more detail regarding these methods.
#'   }
#'
#' @examples
#' \dontrun{
#' df <- pur_data(years = 2000, chemicals = "methyl bromide")
#' df2 <- pur_data(years = 1990:1995, chemicals = c("methyl bromide",
#'                 "trifluralin"), unit = "township")
#' df3 <- pur_data(years = 2010:2012, chemicals = "ammonia", sum = FALSE)
#' }
#' @importFrom dplyr %>%
#' @export
pur_data <- function(county, years, chemicals, sum = TRUE, unit = "section") {

  if (length(years) == 1) {
    if (toupper(years) == "ALL") {
      years <- 1990:2015
    }
  }

  for (i in 1:length(years)) {

    raw_df <-

  }

}























