#' purexposure: A package for working with CA Pesticide Use Registry data.
#'
#' The purexposure package provides functions to pull data from California's
#' Pesticide Use Registry (PUR), as well as to calculate exposure to and
#' visualize active ingredients present in applied pesticides.
#'
#' @section pull_ functions
#' The pull_ functions are meant to ...
#' \itemize{
#'   \item{pull_pur_file}{pull one raw PUR file}
#'   \item{pull_raw_pur}{pull raw PUR files for multiple years and counties}
#' }
#'
#' @section clean_* function
#' clean_pur_data: pull and clean PUR data
#'
#' @section calculate_* functions
#' calculate_exposure: calculate exposure @ one location/time period/buffer
#' calculate_summary_statistics: calculate mean, sd, min, max, etc. for
#' application or exposure for period(s) of time
#'
#' @section map_* functions
#' map_county_outline: map of CA with specified counties shaded in
#' map_county_application: for one county- application per section or township
#' for speficied period of time
#' map_exposure: for one location/buffer radius- exposure for specified time period
#' map_application: for one location/buffer radius- exposure for specified time
#' period
#'
#' @section plot_* functions
#' plot_application_timeseries: timeseries plot of application over time for
#' a particular geographic unit (county/counties, township, section) and a
#' particular chemical/class/all
#' plot_exposure_timeseries: timeseries plot of exposure over time for a
#' particular location and a particular chemical/class/all
#'
#'
#' @docType package
#' @name purexposure
NULL
