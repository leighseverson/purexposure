#' California Pesticide Use Report county codes.
#'
#' A data frame containing California county names and corresponding codes
#' used to identify counties in California Pesticide Use Reports. This file,
#' "county.txt" was pulled from the .zip file "pur2000.zip" found here:
#' \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives}
#'
#' @format A data frame with 59 rows and two columns:
#' \describe{
#'   \item{county_name}{A character vector giving California county names.}
#'   \item{county_code}{A character vector giving county codes (ranging from "01"
#'   through "58", with "-1" indicating "unknown") corresponding to each
#'   California county. Note: these codes are unique to California PUR datasets;
#'   they do not correspond to FIPS codes.}
#' }
#'
#' @source
#' \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives}
"county_codes"

#' California Pesticide Use Report chemical codes.
#'
#' A list of data frames (one for each year from 1990 through 2015) containing
#' California Department of Pesticide Regulation chemical codes and names used
#' to identify active ingredients in Pesticide Use Report data. "chemical.txt"
#' files for each year were pulled from the .zip files "pur1990.zip" through
#' "pur2015.zip" found here:
#' \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives}
#'
#' @format A list of 26 elements. Each element is a data frame with a variable
#' number of rows ranging from 3,579 to 3,934 and two columns:
#' \describe{
#'   \item{chem_code}{An integer giving the chemical code. This uniquely
#'   identifies a chemical within a year.}
#'   \item{chemname}{A character vector giving common chemical name for each
#'   active ingredient. These are usually listed on the pesticide product
#'   label.}
#' }
#'
#' @source
#' \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives}
"chemical_list"

#' California state SpatialPolygonsDataFrame object.
#'
#' A SpatialPolygonsDataFrame object for the outline of the state of California.
#' Downloaded and subset from the 2016 U.S. Census Cartographic Boundary
#' Shapefile for states.
#'
#' @format A SpatialPolygonsDataFrame object at the 1:20,000,000 resolution level.
#'
#' @source
#' \url{https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html}
"california_shp"
