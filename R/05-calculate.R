#' Calculate exposure to active ingredients present in applied pesticides.
#'
#' For a particular location, buffer radius from that location, time period, and
#' active ingredient or class of active ingredients, \code{calculate_exposure}
#' returns an estimate of exposure in kg of active ingredient per m^2.
#'
#' @param location A character string. Either a California address inclding
#'   street name, city, state, and zip code, or a pair of coordinates in the
#'   form "longitude, latitude".
#' @param clean_pur_df A data frame returned by \code{clean_pur_data} that
#'   includes data relevant to the county in which your location is located (you
#'   can use the \code{find_location_county} function to figure this out), the
#'   time period, and the active ingredients or chemical classes for which you
#'   want to calculate exposure.
#' @param radius A numeric value greater than zero that gives the radius in meters
#'   defining the buffer around your location in which you would like to
#'   calculate exposure.
#' @param time_period Optional. A character string giving a time period over which you
#'   would like to calculate exposure. For example, if you enter "6 months" for
#'   \code{time_period}, \code{calculate_exposure} will calculate exposure for
#'   every six month period starting from the earliest date present in the
#'   \code{clean_pur_df} data frame. Start and end dates can be optionally specified
#'   with the \code{start_date} and \code{end_date} arguments. Alternatively, to
#'   calculate exposure over only one time period, you can leave this argument
#'   NULL and specify start and end dates.
#' @param start_date Optional. "yyyy-mm-dd" specifying the start date for
#'   exposure estimation. This date should be present in the \code{clean_pur_df}
#'   data frame.
#' @param end_date Optional. "yyyy-mm-dd" specifying the end date for exposure
#'   estimation. This date should be present in the \code{clean_pur_df}
#'   data frame.
#' @param exposure A character string indicating active ingredients for which you
#'   would like to calculate exposure. The default is "all", which will calculate
#'   exposure to the summed active ingredients present in the \code{clean_pur_df}
#'   data frame. You can also enter a particular active ingredient that's present
#'   in your \code{clean_pur_df} data frame, or "chemical_class" to calculate
#'   exposure to each of the chemical classes present in the \code{chemical_class}
#'   column of your \code{clean_pur_df} data frame.
#'
#' @return A data frame with ..
#'
#'
#' @section Note:
#' If the \code{time_period}, \code{start_date}, and \code{end_date} arguments
#' are all left as NULL (their defaults), then exposure will be estiamted across
#' the entire date range of the \code{clean_pur_df} data frame.
#'
#' @examples
#' \dontrun{
#'
#' pur_data <- clean_pur_data()
#'
#' # Time periods
#'
#' # Time periods with specified start and end date
#'
#' # Single time period
#'
#' # All active ingredients
#'
#' # A single active ingredient
#'
#' # By chemical class
#' pur_data_classes <- clean_pur_data()
#'
#' }
#' @export
calculate_exposure <- function(clean_pur_df, location, radius,
                               time_period = NULL, start_date = NULL,
                               end_date = NULL, exposure = "all") {}

























calculate_summary_statistics <- function() {}
