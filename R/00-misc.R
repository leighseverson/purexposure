utils::globalVariables(c("DDLAT", "DDLONG", "MTR", "MTRS", "acre_treated",
                         "aer_gnd_ind", "aerial_ground", "all_missing", "applic_dt",
                         "area", "base_ln_mer", "category", "chem_code", "chemical",
                         "chemical_class", "chemicals", "chemname", "comtrs",
                         "county_cd", "county_code", "county_name", "data", "dist",
                         "end_date", "error_flag", "exposure", "group", "group_by",
                         "id", "kg", "kg_chm_used", "kg_intersection", "kg_perday",
                         "lat", "lbs_chm_used", "lbs_per_acre", "location", "long",
                         "max_lat", "max_long", "none_recorded", "outlier",
                         "perc_fill", "percent", "prodno", "prodstat_ind",
                         "product_name", "radius", "range_dir", "scale_fill", "sd",
                         "section", "signlwrd_ind", "start_date", "township",
                         "township_pad", "tship_dir", "unit_treated", "use_no",
                         "x", "y", "year", "pur_code", "fips_code", "product",
                         "geocode_quiet", "latlon_loc", "n", "n_row",
                         "product_df", "raw_pur", "spdf"))

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
#' library(magrittr)
#' \dontshow{
#' fresno <- readRDS(system.file("extdata", "fresno_spdf.rds", package = "purexposure"))
#' fresno %>% spdf_to_df %>% df_plot()}
#' \donttest{
#' pull_spdf("fresno") %>% spdf_to_df() %>% df_plot()
#' pull_spdf("san diego", "township") %>%
#'    spdf_to_df() %>%
#'    df_plot()
#' }
#' @export
df_plot <- function(df) {

  plot <- ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat,
                                                  group = group)) +
    ggplot2::geom_polygon(color = "black", fill = NA) +
    ggplot2::theme_void() +
    ggplot2::coord_map()

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
#' library(magrittr)
#' \dontshow{
#' df <- readRDS(system.file("extdata", "fresno_spdf.rds", package = "purexposure")) %>%
#'    spdf_to_df()}
#' \donttest{
#' df <- pull_spdf("frenso") %>% spdf_to_df()
#' df2 <- pull_spdf("sonoma") %>% spdf_to_df()
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
#' @examples
#' library(magrittr)
#' tibble::tibble(x = 1:3) %>% tibble_to_vector()
#' @importFrom magrittr %>%
#' @export
tibble_to_vector <- function(tib) {

  vec <- tib %>% dplyr::pull(1) %>% as.character()

  return(vec)

}

scale_fill_gradientn2 <- function(..., colours, values = NULL, space = "Lab",
                                  na.value = "grey50", guide = "colourbar", colors,
                                  alpha = NULL) {
  # Include alpha in ggplot2::scale_fill_gradientn()
  colours <- if (missing(colours)) colors else colours

  ggplot2::continuous_scale("fill", "gradientn",
                            gradient_n_pal2(colours, values, space, alpha = alpha),
                            na.value = na.value, guide = guide, ...)
}

gradient_n_pal2 <- function(colours, values = NULL, space = "Lab", alpha = NULL) {

  # Include alpha option in scales::gradient_n_pal().
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
