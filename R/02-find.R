#' Pull active ingredient chemical codes from PUR Chemical Lookup Tables.
#'
#' For a vector of chemical names, \code{find_chemical_codes} returns
#' a data frame with corresponding chemical codes from the PUR Chemical Lookup
#' Table for a given year. This function uses pattern matching to return results.
#' As a starting place, or for more thorough classifications, see the CA
#' Department of Pesticide Regulation's Summary of Pesticide Use Report Data,
#' Indexed by Chemical (2008):
#' \url{http://www.cdpr.ca.gov/docs/pur/pur08rep/chmrpt08.pdf}
#'
#' @param years A vector of four-digit numeric years in the range of 1990 to
#'   2015. Indicates the years in which you would like to match chemical codes.
#' @param chemicals A string or vector of strings giving search terms of
#'   chemicals to match with active ingredients present in pesticides applied
#'   in the given year. The default value is "all", which returns codes for all
#'   active ingredients applied in a given year.
#' @param by_year TRUE / FALSE for whether you would like PUR Chemical Lookup
#'   Tables separated by year (in a `year` column). If `by_year` is `FALSE`, the
#'   default, a data frame is returned with unique results from all years given
#'   in the `years` argument.
#'
#' @return A data frame:
#'   \describe{
#'     \item{chem_code}{An integer value with chemical codes corresponding to
#'     each active ingredient. \code{chem_code} values are used to later filter
#'     raw PUR data sets.}
#'     \item{chemname}{A character string giving unique active ingredients
#'     corresponding to each search term.}
#'     \item{chemical}{A character string with search terms given in the
#'     \code{chemicals} argument. Not included if the `chemicals` argument is
#'     set to its default value of "all".}
#'     \item{year}{Included if `by_year` is set to `TRUE`.}
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
#' @importFrom magrittr %>%
#' @export
find_chemical_codes <- function(years, chemicals = "all", by_year = FALSE) {

  df <- purexposure::chemical_list

  df <- df[names(df) %in% years]

  for (i in 1:length(years)) {
    df[[i]] <- df[[i]] %>% dplyr::mutate(year = years[i])
  }

  df <- do.call("rbind", df)

  if (!"all" %in% chemicals) {
    out <- purrr::map_dfr(chemicals, help_find_chemical, df)  %>%
      unique() %>%
      dplyr::select(chem_code, chemname, chemical, year)
  } else {
    out <- purrr::map_dfr(chemicals, help_find_chemical, df)  %>%
      unique() %>%
      dplyr::select(chem_code, chemname, year)
  }

  if (!by_year) {
    out <- out %>% dplyr::select(-year) %>% unique()
  }

  return(out)

}

#' Find pesticide product names and registration numbers from PUR Product Lookup
#' Tables.
#'
#' For a vector of years and product search terms, \code{find_product_name}
#' returns a data frame with corresponding product registration numbers,
#' \code{prodno}, indicator codes, and product names.
#'
#' Product tables are pulled by year from the CDPR's FTP server. Downloaded
#' tables are saved in a temporary environment, which is deleted at the end of
#' the current R session.
#'
#' @param products A character string or a vector of character strings with
#'   pesticide product names that you would like to search for. Not case
#'   sensitive. The default is "all", which will return all pesticide products
#'   applied for a given year.
#' @inheritParams pull_product_table
#' @param by_year TRUE / FALSE for whether you would like PUR Product Lookup
#'   Tables separated by year (in a `year` column). If `by_year` is `FALSE`, the
#'   default, a data frame is returned with unique results from all years given
#'   in the `years` argument.
#'
#' @return A data frame with seven columns:
#' \describe{
#'   \item{prodno}{The CA registration number. Can be matched with the
#'   \code{prodno} in a raw or cleaned PUR data set.}
#'   \item{prodstat_ind}{Character. An indication of product registration status:
#'   \itemize{
#'   \item A = Active
#'   \item B = Inactive
#'   \item C = Inactive, Not Renewed
#'   \item D = Inactive, Voluntary Cancellation
#'   \item E = Inactive, Cancellation
#'   \item F = Inactive, Suspended
#'   \item G = Inactive, Invalid Data
#'   \item H = Active, Suspended}}
#'   \item{product_name}{Character. The name of the product taken from the
#'   registered product label. May have been modified by DPR's Registration Branch
#'   to ensure uniqueness.}
#'   \item{signlwrd_ind}{Integer. The signal word printed on the front of the
#' product label:
#'   \itemize{
#'   \item 1 = Danger (Poison)
#'   \item 2 = Danger (Only)
#'   \item 3 = Warning
#'   \item 4 = Caution
#'   \item 5 = None}}
#'   \item{product}{Product name search terms.}
#'   \item{year}{The year for which product table information was pulled.
#'   Included if `by_year` is set to TRUE.}
#' }
#'
#' @examples
#' prod_df <- find_product_name(2000, "mosquito")
#' \donttest{
#' prod_df2 <- find_product_name(2010, c("insecticide", "rodenticide"))
#' }
#' @export
find_product_name <- function(years, products = "all", quiet = FALSE,
                              by_year = FALSE) {

  prod_df <- pull_product_table(years, quiet = quiet)

  for (i in 1:length(products)) {
    df <- help_find_product(products[i], prod_df)
    if (i == 1) {
      out <- df
    } else {
      out <- rbind(out, df)
    }
  }

  out <- unique(out) %>% dplyr::select(1:4, product, year)

  if (!by_year) {
    out <- out %>% dplyr::select(-year) %>% unique()
  }

  return(out)

}

#' Find California county codes or names.
#'
#' Given a vector of counties, \code{find_counties} returns either PUR
#' county codes or names.
#'
#' @param counties A vector of character strings giving either a county names,
#'  two digit PUR county codes, or six-digit FIPS county codes. Not case
#'  sensitive. California names and county codes as they appear in PUR data sets
#'  can be found in the \code{county_codes} data set available with this package.
#' @param return Either "pur_codes" to return PUR county codes (the default),
#'  "fips_codes" to return FIPS county codes, or "names" to return county names.
#'
#' @return If \code{return = "pur_codes"}, a vector of two-character strings giving
#'   the corresponding PUR county codes. If \code{return = "fips_codes"}, a vector
#'   of six-digit character strings giving the corresponding FIPS county codes.
#'   If \code{return = "names"}, a vector of county names.
#'
#' @examples
#' find_counties(c("01", "06005", "el dorado"))
#' \donttest{
#' find_counties(c("01", "06005", "el dorado"), return = "fips_codes")
#' find_counties(c("01", "06005", "el dorado"), return = "names")
#' }
#' @importFrom magrittr %>%
#' @export
find_counties <- function(counties, return = "pur_codes") {

  for (i in 1:length(counties)) {

    county_name <- help_find_code(counties[i], return)
    if (is.null(county_name)) {

      stop(paste0("\"", counties[i], "\"", " doesn't match any ",
                  "California counties. \nCheck out the ",
                  "county_codes data set included with this ",
                  "package for county names and corresponding ",
                  "codes."))

    } else {

      if (i == 1) {
        out <- county_name
      } else {
        out <- c(out, county_name)
      }

    }
  }

  return(out)

}

#' Find the county from an address or coordinate pair.
#'
#' Given a California address or longitude/latitude coordinates,
#' \code{find_location_county} returns the corresponding California county or
#' PUR code.
#'
#' @param locations A vector of character strings. Each location should be
#'   either a California address including street name, city, state, and
#'   5-digit zip code, or a pair of coordinates in the form "longitude,
#'   latitude".
#' @param return Either "name" to return county name (the default), "pur_code"
#'   to return PUR county code, or "fips_code" to return the FIPS county code.
#' @param ... Used internally.
#'
#' @return A character string giving the California county where the address or
#' coordinate pair given in \code{location} is located.
#'
#' @examples
#' address <- "13883 Lassen Ave, Helm, CA 93627"
#' long_lat <- c("-120.09789, 36.53379")
#' find_location_county(c(address, long_lat))
#' @export
find_location_county <- function(locations, return = "name", ...) { #latlon_out

  args <- list(...)

    if (!is.null(args$latlon_out)) {

      out <- purrr::map_dfr(locations, help_find_location_county, return = return,
                            latlon_out = args$latlon_out)
    } else {

      out <- purrr::map_dfr(locations, help_find_location_county, return = return)

    }

  return(out)

}
