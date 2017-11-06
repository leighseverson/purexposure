#' Plot data frame spatial objects.
#'
#' \code{df_plot} plots a data frame spatial object. (A
#' SpatialPolygonsDataFrame that has been "tidied" using the broom package.)
#' Meant to be analogous to the ease of using plot() to quickly view a
#' SpatialPolygonDataFrame object.
#'
#' @param df A data frame returned from the \code{spdf_to_df} function.
#'
#' @return A ggplot2 plot of the county.
#'
#' @examples
#' \dontrun{
#' pull_spdf("san diego", "township") %>%
#'    spdf_to_df() %>%
#'    df_plot()
#' }
#' @export
df_plot <- function(df) {

  plot <- ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat,
                                                  group = group)) +
    ggplot2::geom_polygon(color = "black", fill = NA) +
    ggplot2::theme_void()

  return(plot)

}

#' Convert county SpatialPolygonsDataFrame to a tidy data frame.
#'
#' \code{spdf_to_df} converts a SpatialPolygonsDataFrame object returned from
#' the \code{pull_spdf} function to a data frame.
#'
#' @param spdf A SpatialPolygonsDataFrame object returned from
#' the \code{pull_spdf} function.
#'
#' @return A data frame with 24 columns if the \code{spdf} object is on the
#' section level and 23 columns if the \code{spdf} object is on the township
#' level.
#'
#' @examples
#' \dontrun{
#' df <- pull_spdf("fresno") %>% spdf_to_df()
#' df2 <- pull_spdf("sonoma") %>% spdf_to_df()
#'
#' # use df_plot() function to easily plot the output data frames:
#' df_plot(df)
#' df_plot(df2)
#' }
#' @importFrom magrittr %>%
#' @export
spdf_to_df <- function(spdf) {

  df <- suppressMessages(sp::merge(broom::tidy(spdf), as.data.frame(spdf),
                                   by.x = "id", by.y = 0))

  if ("MTRS" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(MTRS = as.character(MTRS))
  }
  if ("MTR" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(MTR = as.character(MTR))
  }

  return(df)

}

#' Calculate euclidean distance between two points.
#'
#' \code{euc_distance} calculates the straight-line distance between
#' two points.
#'
#' This is a helper function for \code{calculate_exposure}.
#'
#' @param long Longitude (x) of second point
#' @param lat Latitude (y) of second point
#' @param origin_long Longitude (x) of first point
#' @param origin_lat Latitude (y) of first point
#'
#' @return A data frame with one row and three columns: \code{long} and
#' \code{lat} give the second point's coordinates, and \code{dist} gives the
#' euclidian distance from these coordinates from the origin.
#'
#' @example{
#' euc_distance(-120, 36, 120.5, 37.5)
#' }
#' @export
euc_distance <- function(long, lat, origin_long, origin_lat) {

  x <- abs(lat - origin_lat)
  y <- abs(long - origin_long)
  dist <- sqrt((x^2) + (y^2))
  out <- data.frame(long = long, lat = lat, dist = dist)

  return(out)

}

#' Return a character vector from a tibble with one column.
#'
#' \code{tibble_to_vector} takes a tibble with one column and returns the
#' values in that column as a character vector.
#'
#' This is a helper function for \code{pull_raw_pur}, \code{pull_clean_pur},
#' and \code{pur_filt_df}.
#'
#' @param tib A tibble with only one column.
#'
#' @return A character vector.
#'
#' @example{
#' tibble::tibble(x = 1:3) %>% tibble_to_vector()
#' }
#'
#' @importFrom magrittr %>%
#' @export
tibble_to_vector <- function(tib) {

  vec <- tib %>% dplyr::pull(1) %>% as.character()

  return(vec)

}

#' Include alpha in ggplot2::scale_fill_gradientn().
#'
#' This function adds an `alpha` argument to scale_fill_gradientn() from the
#' ggplot2 package.
#' @export
scale_fill_gradientn2 <- function(..., colours, values = NULL, space = "Lab",
                                  na.value = "grey50", guide = "colourbar", colors,
                                  alpha = NULL) {
  colours <- if (missing(colours)) colors else colours

  ggplot2::continuous_scale("fill", "gradientn",
                            gradient_n_pal2(colours, values, space, alpha = alpha),
                            na.value = na.value, guide = guide, ...)
}

#' Include alpha option in scales::gradient_n_pal().
#'
#' This function adds an "alpha" argument from gradient_n_pal() from the scales
#' package.
#' @export
gradient_n_pal2 <- function(colours, values = NULL, space = "Lab", alpha = NULL) {
  if (!identical(space, "Lab")) {
    warning("Non Lab interpolation is deprecated", call. = FALSE)
  }
  ramp <- scales::colour_ramp(colours, alpha = alpha)

  function(x) {
    if (length(x) == 0) return(character())

    if (!is.null(values)) {
      xs <- seq(0, 1, length.out = length(values))
      f <- stats::approxfun(values, xs)
      x <- f(x)
    }

    ramp(x)
  }
}
