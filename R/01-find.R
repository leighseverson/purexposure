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
#' @importFrom magrittr %>%
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

#' Find California county codes or names
#'
#' Given a vector of counties, \code{find_counties} returns either PUR
#' county codes or names.
#'
#' @param county A vector of character strings giving either a county names or
#'  two digit PUR county codes. Not case sensitive. California names and county
#'  codes as they appear in PUR datasets can be found in the \code{county_codes}
#'  dataset available with this package.
#' @param return Either "codes" to return county codes (the default) or "names"
#'  to return county names.
#'
#' @return If \code{return = "codes"}, a vector of two-character strings giving
#'   the corresponding PUR county codes. If \code{return = "names"}, a vector
#'   of county names.
#'
#' @examples
#' find_counties(c("01", "03", "el dorado"))
#' find_counties(c("contra costa", "45"))
#'
#' find_counties(c("01", "03", "el dorado"), return = "names")
#' find_counties(c("contra costa", "45"), return = "names")
#'
#' find_counties("blah")
#' @importFrom magrittr %>%
#' @export
find_counties <- function(counties, return = "codes") {

  find_county_code <- function(county, find = return) {

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

  # purrr::map wasn't working v well w/ an error message..

  for (i in 1:length(counties)) {
    county_name <- find_county_code(counties[i])
    if (is.null(county_name)) {
      stop(paste0("\"", counties[i], "\"", " doesn't match any ",
                  "California counties. \nCheck out the ",
                  "county_codes dataset included with this ",
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
#' @inheritParams calculate_exposure
#' @param return Either "name" to return county name (the default) or "code"
#'  to return county code.
#'
#' @return A character string giving the California county where the address or
#' coordinate pair given in \code{location} is located.
#'
#' @examples
#' \dontrun{
#' address <- "13883 Lassen Ave, Helm, CA 93627"
#' find_location_county(location = address)
#'
#' long_lat <- c("-120.09789, 36.53379")
#' find_location_county(location = long_lat)
#' }
#' @export
find_location_county <- function(location, return = "name") {

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

  counties <- maps::map("county", fill = TRUE, col = "transparent", plot = FALSE)
  ids <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- maptools::map2SpatialPolygons(counties, IDs = ids,
                                               proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

  points_sp <- sp::SpatialPoints(data.frame(x = latlon_out[1],
                                            y = latlon_out[2]),
                                 proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

  index <- sp::over(points_sp, counties_sp)

  county_names <- sapply(counties_sp@polygons, function(x) x@ID)
  county_name <- county_names[index]

  county_name <- strsplit(county_name, ",")[[1]][2]

  find_counties_safe <- purrr::safely(find_counties)
  return <- paste0(return, "s")
  name_clean <- find_counties_safe(county_name, return = return)

  if (!is.null(name_clean$error)) {
    stop(paste0("Couldn't find ", "\"", location,  "\"", " in California."))
  }

  return(name_clean$result)

}
