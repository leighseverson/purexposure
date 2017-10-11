#' Pull active ingredient chemical codes from PUR Chemical Lookup Tables.
#'
#' For a vector of chemical names, \code{find_chemical_codes} returns
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
#'   \describe{
#'     \item{chem_code}{An integer value with chemical codes corresponding to
#'     each active ingredient. \code{chem_code} values are used to later filter
#'     raw PUR datasets.}
#'     \item{chemname}{A character string giving unique active ingredients
#'     corresponding to each search term.}
#'     \item{chemical}{A character string with search terms given in the
#'     \code{chemicals} argument.}
#' }
#'
#' @section Note:
#' The PUR Chemical Lookup Table for a year lists all active ingredients present
#' in applied pesticides across the state of California. Therefore, PUR data for
#' a particular county may not include records for active ingredients returned
#' by \code{find_chemical_codes} for the same year.
#'
#' @examples
#' find_chemical_codes(2000, "methyl bromide")
#' find_chemical_codes(1995, c("ammonia", "benzene"))
#' @importFrom dplyr %>%
#' @export
find_chemical_codes <- function(year, chemicals = "all") {

  df <- purexposure::chemical_list
  df <- df[[as.character(year)]]

  pull_chemical_code <- function(chemical) {

    quotemeta <- function(string) {
      stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
    }

    chem_up <- toupper(chemical)
    chem_up <- substr(chem_up, 1, 50)
    if (chem_up == "ALL") {
      df2 <- df
    } else {
      df2 <- df[grep(quotemeta(chem_up), df$chemname), ]
      df2 <- df2 %>% dplyr::mutate(chemical = chemical)
    }

    return(df2)

  }

  out <- purrr::map_dfr(chemicals, pull_chemical_code)  %>%
    unique()

  return(out)

}

#' Find California county codes
#'
#' Given a vector of counties, \code{find_county_codes} returns PUR county codes.
#'
#' @param county A vector of character strings giving either a county names or
#'  two digit PUR county codes. Not case sensitive. California names and county
#'  codes as they appear in PUR datasets can be found in the county_codes dataset
#'  available with this package.
#'
#' @return A vector of two-character string giving the corresponding PUR county
#'   codes.
#'
#' @examples
#' find_county_codes(c("01", "03", "el dorado"))
#' find_county_codes(c("contra costa", "45"))
#' @export
find_county_codes <- function(counties) {

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

  codes <- purrr::map(counties, find_county_code) %>% unlist()

  return(codes)

}
