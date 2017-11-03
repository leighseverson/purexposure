#' purexposure: A package for working with CA Pesticide Use Registry data.
#'
#' The purexposure package provides functions to pull data from California's
#' Pesticide Use Registry (PUR), as well as to calculate exposure to and
#' visualize active ingredients present in applied pesticides. Functions are
#' organized into \code{find_*}, \code{pull_*}, \code{calculate_*}, and
#' \code{plot_*}. There are a few main functions from each category:
#'
#' @section find_* functions:
#' The code{find_} functions help with searches of PUR chemical, county, and
#' product codes, as well as the county in which an address or coordinate pair
#' is located.
#' \describe{
#'   \item{find_chemical_codes}{Pull active ingredient chemical codes from PUR
#'   Chemical Lookup Tables.}
#'   \item{find_counties}{Find California county codes or names.}
#'   \item{find_product_name}{Find Pesticide Product names and registration
#'   numbers from PUR Product Lookup Tables.}
#' }
#'
#' @section pull_* functions:
#' The \code{pull_} functions facilitate downloading data from the CA Department
#' of Pesticide Regulation's website.
#' \describe{
#'   \item{pull_raw_pur}{Pull raw PUR data by counties and years.}
#'   \item{pull_clean_pur}{Pull cleaned PUR data by counties, years, and active
#'   ingredients.}
#' }
#'
#' @section calculate_* functions:
#' The \code{calculate_exposure} function calculates exposure (in kg/m^2) to
#' applied pesticides for a given location, buffer extending from that location,
#' time period, and active ingredients.
#'
#' @section plot_* functions:
#' The \code{plot_} functions help with visualizations of application.
#' \describe{
#'   \item{plot_county_application}{Plot pesticide application by county, summed
#'   by section or township.}
#'   \item{plot_exposure}{Plot exposure to applied pesticides at a particular
#'   location.}
#'   \item{plot_application_timeseries}{Plot time series of active ingredients
#'   in applied pesticides.}
#' }
#'
#' @docType package
#' @name purexposure
NULL
