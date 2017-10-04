#' California Pesticide Use Report county codes.
#'
#' A data frame containing California county names and corresponding codes
#' used to identify counties in California Pesticide Use Reports. This file,
#' "county.txt" was pulled from the .zip file "pur2000.zip" found here:
#' \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives}
#'
#' @format A data frame with 59 rows and 2 columns:
#' \describe{
#'   \item{county_name}{A character vector giving California county names}}
#'   \item{county_code}{A character vector giving county codes (ranging from "01"
#'   through "58", with "-1" indicating "unknown") corresponding to each
#'   California county. Note: these codes are unique to California PUR datasets;
#'   they do not correspond to FIPS codes.}
#' }
#'
#' @source
#' \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives}
