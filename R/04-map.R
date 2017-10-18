#' Map a county's location in California
#'
#' \code{map_counties} returns one or multiple ggplots with county
#' locations in California given either a vector of county names or codes,
#' or a PUR data frame with a \code{county_cd}, \code{county_name}, or
#' \code{county_code} column (A data frame returned from either
#' \code{pull_pur_file}, \code{pull_raw_pur}, or \code{pull_clean_pur}).
#'
#' @param counties_or_df A character vector of county names or county codes.
#'   You can use the \code{county_codes} dataset included with this package to
#'   check out PUR county names and codes. This argument can also be a data frame
#'   with a \code{county_cd}, \code{county_name}, or \code{county_code} column. If
#'   you provide a data frame, a plot for every county with data in that dataset
#'   will be output.
#' @param one_plot TRUE / FALSE. If you provided multiple counties, whether you
#'   would like county outlines plotted in the same plot (TRUE), or if you would
#'   like separate plots returned in a list (FALSE). The default is TRUE
#' @param fill_color A character string giving either a ggplot2 color or a
#'   hex color code ("#0000FF", for example). The default is "red".
#' @param transparency A numeric value in [0,1]. Corresponds to the
#'   \code{ggplot2::alpha} transparency argument. Numbers closer to 0 will result
#'   in more transparency. The default is 0.5.
#'
#' @return A ggplot or a list of ggplots of Califnornia with shaded-in counties.
#' List element names correspond to county names.
#'
#' @examples
#' \dontrun{
#' map_counties("fresno")
#'
#' pur_df <- pull_clean_pur(1990, counties = c("01", "05", "12"), verbose = FALSE)
#' map_counties(pur_df)
#'
#' plot_list <- map_counties(c("san bernardino", "ventura"), one_plot = TRUE)
#' names(plot_list)
#' plot_list[[1]]
#' plot_list[[2]]
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang !!
#' @export
map_counties <- function(counties_or_df, one_plot = TRUE, fill_color = "red",
                         transparency = 0.5) {

  ca_shp <- purexposure::california_shp
  ca_df <- spdf_to_df(ca_shp)

  if (!is.vector(counties_or_df) & !is.data.frame(counties_or_df)) { #overkill
    stop(paste0("The counties_or_df argument should be either a character",
                " vector of county names or codes or a PUR data frame."))
  }

  if (is.vector(counties_or_df)) {
    counties <- find_counties(counties_or_df, return = "names")
  }

  if (is.data.frame(counties_or_df)) {
    check <- any(c("county_cd", "county_name", "county_code") %in%
                     colnames(counties_or_df))
    if (!check) {
      stop(paste0("The counties_or_df data frame should have either a county_cd,",
                  " county_name, or county_code column.\nThis data frame should be",
                  " returned from either pull_pur_file(), pull_raw_pur(), or",
                  " pull_clean_pur()."))
    }
      county_col <- grep("county_code", colnames(counties_or_df), value = TRUE)
      if (length(county_col) == 0) {
        county_col <- grep("county_name", colnames(counties_or_df), value = TRUE)
        if (length(county_col) == 0) {
          county_col <- grep("county_cd", colnames(counties_or_df), value = TRUE)
        }
      }
      counties <- counties_or_df %>%
        dplyr::select(!!county_col) %>%
        unique() %>%
        tibble_to_vector()
  }

  # pull county shapefiles
  county_shps <- purrr::map(counties, pull_spdf)
  county_dfs <- suppressWarnings(purrr::map(county_shps, spdf_to_df))

  ca <- df_plot(ca_df)
  plot <- ca + ggplot2::geom_polygon(data = county_dfs[[1]],
                                     ggplot2::aes(x = long, y = lat, group = group),
                                     color = "transparent", fill = fill_color, alpha =
                                       transparency)

  if (!one_plot) {

    if (length(counties) > 1) {
      out <- list()
      out[[1]] <- plot
      for (i in utils::tail(1:length(counties), -1)) {
        out[[i]] <- ca + ggplot2::geom_polygon(data = county_dfs[[i]],
                                               ggplot2::aes(x = long, y = lat, group = group),
                                               color = "transparent", fill = fill_color, alpha =
                                                 transparency)
      }
      names(out) <- counties
    } else {
      out <- plot
    }

  } else {

    if (length(county_dfs) > 1) {
      for (i in utils::tail(1:length(counties), -1)) {
        plot <- plot + ggplot2::geom_polygon(data = county_dfs[[i]],
                                           ggplot2::aes(x = long, y = lat, group = group),
                                           color = "transparent", fill = fill_color, alpha =
                                             transparency)
        out <- plot

      }
    } else {
      out <- plot
    }

  }

  return(out)

}

map_county_application <- function() {}

map_exposure <- function() {}

map_application <- function() {}


