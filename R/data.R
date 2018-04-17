#' California Pesticide Use Report county codes.
#'
#' A data frame containing California county names and corresponding PUR and
#' FIPS codes. PUR county codes are unique to PUR datasets, and FIPS county codes
#' are U.S. Federal Information Processing Standard codes. The file with county
#' names and PUR codes, "county.txt", was pulled from the .zip file
#' "pur2000.zip" found here:
#' \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives}. FIPS codes were
#' pulled from here:
#' \url{http://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt}
#'
#' @format A data frame with 58 rows and three columns:
#' \describe{
#'   \item{county_name}{A character vector giving California county names.}
#'   \item{pur_code}{A character vector giving two-digit PUR county codes (ranging
#'   from "01" through "58") corresponding to each California county. Note:
#'   these codes are unique to California PUR datasets; they do not correspond
#'   to FIPS codes.}
#'   \item{fips_code}{A character vector giving six-digit FIPS county codes
#'   corresponding to each California county.}
#' }
#'
#' @source
#' \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives}
#' \url{http://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt}
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

#' A toy raw PUR dataset
#'
#' A PUR dataset pulled using `pull_raw_pur` for Fresno county for the year
#' 2000 and filtered to the month of January.
#'
#' @format A data frame with 18,062 rows and 33 columns.
"fresno_raw"

#' A toy clean PUR dataset
#'
#' A PUR dataset pulled using `pull_clean_pur` for Fresno county for the year
#' 2000 and filtered to the month of January.
#'
#' @format A data frame with 16,907 rows and 13 columns.
"fresno_clean"

#' A toy exposure list
#'
#' A list created using the `calculate_exposure` function for the `fresno_ex`
#' dataset, the location (longitude, latitude) "-119.726751, 36.660967", and
#' a radius of 3,000 meters. The `county_plot` list element has been removed.
#'
#' @format A list with four data frame elements: `exposure`, `meta_data`,
#' `buffer_plot_df`, and `clean_pur_df`.
"exposure_ex"

#' Another toy exposure list
#'
#' Another list created using the `calculate_exposure` function for the
#' `fresno_ex` dataset, the location (longitude, latitude)
#' "-119.247100, 37.204875", and a radius of 3,000 meters. The `county_plot`
#' list element has been removed.
#'
#' @format A list with four data frame elements: `exposure`, `meta_data`,
#' `buffer_plot_df`, and `clean_pur_df`.
"exposure_ex2"

#' SpatialPolygonsDataFrame describing Fresno county sections
#'
#' A SpatialPolygonsDataFrame object pulled using the `pull_spdf` function for
#' Fresno county sections.
"fresno_spdf"
