#' purexposure: A package for working with CA Pesticide Use Registry data.
#'
#' The \code{purexposure} package provides functions to pull data from California's
#' Pesticide Use Registry (PUR), as well as to calculate exposure to and
#' visualize active ingredients present in applied pesticides. Functions are
#' categorized into \code{find_*}, \code{pull_*}, \code{calculate_*},
#' \code{plot_*}, and \code{write_*}. These are the functions from each category:
#'
#' @section find_* functions:
#' \code{find_} functions help with searches of PUR chemical, county, and
#' product codes.
#' \itemize{
#'   \item \code{find_chemical_codes}: Pull active ingredient chemical codes
#'   from PUR Chemical Lookup Tables.
#'   \item \code{find_counties}: Find California county codes or names.
#'   \item \code{find_location_county}: Find the counties of addresses or
#'   coordinates.
#'   \item \code{find_product_name}: Find Pesticide Product names and registration
#'   numbers from PUR Product Lookup Tables.
#' }
#'
#' @section pull_* functions:
#' \code{pull_} functions facilitate downloading data from the CA Department
#' of Pesticide Regulation's website.
#' \itemize{
#'   \item \code{pull_raw_pur}: Pull raw PUR data by counties and years.
#'   \item \code{pull_clean_pur}: Pull cleaned PUR data by counties, years, and
#'   active ingredients.
#'   \item \code{pull_product_table}: Pull PUR Product Tables for a vector of
#'   years.
#'   \item \code{pull_spdf}: Pull section or township-level
#'   SpatialPolygonsDataFrame for a county.
#' }
#'
#' @section calculate_* function:
#' The \code{calculate_exposure} function calculates exposure (in kg/m^2) to
#' applied pesticides for a given location, buffer extending from that location,
#' time period, and active ingredients.
#'
#' @section plot_* functions:
#' \code{plot_} functions help with visualizations of application.
#' \itemize{
#'   \item \code{plot_county_application}: Plot pesticide application by county,
#'   summed by section or township.
#'   \item \code{plot_county_locations}: Plot county locations in California.
#'   \item \code{plot_exposure}: Plot exposure to applied pesticides at a
#'   particular location.
#'   \item \code{plot_application_timeseries}: Plot time series of active
#'   ingredients in applied pesticides.
#'   \item \code{plot_locations_exposure}: Plot exposure for multiple locations
#'   in a given county.
#' }
#'
#' @section write_* function:
#' The \code{write_exposure} function calculates exposure for multiple locaitons
#' and writes out files (exposure values, meta data, and plots) to a specified
#' directory.
#'
#' @docType package
#' @name purexposure
NULL
