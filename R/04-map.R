#' Map a county's location in California
#'
#' \code{map_counties} returns one or multiple plots with county
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

#' Map pesticide application by county
#'
#' \code{map_county_application} returns a plot of applied pesticides (either the
#' sum of all active ingredients present in the input \code{pull_clean_pur} data
#' frame, a specified chemical class, or a specified active ingredient). Application
#' is summed by section or township. PLS units can be shaded by amount or by
#' percentile.
#'
#' @param clean_pur_df A data frame returned by \code{pull_clean_pur}.
#' @param county Optional. If your \code{clean_pur_df} data frame contains data
#'   for multiple counties, this argument specifies which county you would like
#'   to plot application for. Either a PUR county name or county code. California
#'   names and county codes as they appear in PUR datasets can be found in the
#'   county_codes dataset available with this package.
#' @param pls Optional. Either "section" or "township". If your
#'   \code{clean_pur_df} data frame has both a \code{section} and
#'   \code{township} column, the \code{pls} argument specifies which pls unit
#'   you would like to plot application for. If you pulled data specifying
#'   \code{unit = "township"}, application will be plotted by township.
#' @param color_by Either "amount" or "percentile" (the default). Specifies
#'   whether you would like application amounts to be colored according to
#'   amount, resulting in a gradient legend, or by the percentile that they fall
#'   into for the given dataset and date range. You can specify percentile
#'   cutpoints with the \code{percentile} argument.
#' @param percentile A numeric vector in (0, 1) specifying percentile cutpoints.
#'   The default is \code{c(0.25, 0.5, 0.75)}, which results in four categories:
#'   < 25th percentile, >= 25th to < 50th, >= 50th to < 75th, and >= 75th.
#' @param start_date Optional. "yyyy-mm-dd" giving a starting date for the date
#'   range that you would like to map application for. The default is to plot
#'   application for the entire date range in your \code{clean_pur_df} data frame.
#' @param end_date Optional. "yyyy-mm-dd" giving an ending date for the date
#'   range that you would like to plot application for. The default is to plot
#'   application for the entire date range in your \code{clean_pur_df} data frame.
#' @param chemicals Either "all" (the default) to plot summed active ingredents
#'   present in your \code{clean_pur_df} data frame, a chemical class present in
#'   the \code{chemical_class} column of the \code{clean_pur_df} data frame, or
#'   a specific active ingredient present in the \code{chemname} column of the
#'   \code{clean_pur_df} data frame.
#' @param fill_option An argument passed on to the \code{option} argument of
#'   \code{scale_fill_viridis}. The default is "viridis".
#' @param crop TRUE / FALSE for whether you would like your plot zoomed in on
#'   sections or townships with recorded application data.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{plot}{A plot of the county with application summed by section or
#'   township and colored by amount or by percentile.}
#'   \item{data}{A data frame with the plotted application data.}
#'   \item{percentile_values}{A data frame with two columns: \code{percentile}
#'   and \code{kg}, giving the cut points for each percentile in the
#'   \code{clean_pur_df} for the specified chemicals. This element of the list
#'   is not returned if \code{color_by = "amount"}.}
#' }
#'
#' @examples
#' \dontrun{
#' # plot all active ingredients
#' pur_df <- pull_clean_pur(2000:2001, "fresno", verbose = F)
#' fresno_list <- map_county_application(pur_df,
#'                                       percentile = c(0.2, 0.4, 0.6, 0.8))
#' fresno_list$plot
#' head(fresno_list$data)
#' fresno_list$percentile_values
#'
#' # plot a specific active ingredient
#' fresno_list2 <- map_county_application(pur_df, pls = "township",
#'                                        color_by = "amount",
#'                                        chemicals = "sulfur",
#'                                        fill_option = "plasma")
#' fresno_list2$plot
#'
#' # plot a chemical class
#' chemical_class_df <- purrr::map2_dfr(2010, c("methidathion", "parathion",
#'                                              "naled", "malathion",
#'                                              "trichlorfon"),
#'                                      find_chemical_codes) %>%
#'      dplyr::mutate(chemical_class = "organophosphates") %>%
#'      dplyr::select(-chemical)
#' op_yuba <- pull_clean_pur(2010, "yuba",
#'                           chemicals = chemical_class_df$chemname,
#'                           verbose = F, sum_application = T,
#'                           sum = "chemical_class",
#'                           chemical_class = chemical_class_df) %>%
#'    map_county_application(color_by = "amount")
#' op_yuba$plot
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @export
map_county_application <- function(clean_pur_df, county = NULL, pls = NULL,
                                   color_by = "percentile",
                                   percentile = c(0.25, 0.5, 0.75),
                                   start_date = NULL, end_date = NULL,
                                   chemicals = "all", fill_option = "viridis",
                                   crop = FALSE) {

  if (is.null(pls)) {
    if (!all(is.na(clean_pur_df$section))) {
      section_township <- "section"
    } else {
      section_township <- "township"
    }
  } else {
    section_township <- tolower(pls)
  }

  if (!"county_code" %in% colnames(clean_pur_df)) {
    stop(paste0("The clean_pur_df argument should be an unaltered data frame ",
                "returned from the pull_clean_pur() function."))
  }

  # pull county shapefile
  if (is.null(county)) {
    code <- unique(clean_pur_df$county_code)
  } else {
    code <- find_counties(county)
  }

  if (length(code) != 1) {
    stop(paste0("Since there is data for more than one county in your ",
                "clean_pur_df data frame,\nspecify which county you would ",
                "like to plot data for with the county argument."))
  }

  county_shp <- pull_spdf(code, section_township = section_township)
  county_bbox <- as.data.frame(county_shp@bbox)
  county_df <- spdf_to_df(county_shp)

  if (chemicals == "all") {

    pur_df <- clean_pur_df

  } else if (!all(is.na(clean_pur_df$chemical_class))) {

    if (!chemicals %in% unique(clean_pur_df$chemical_class)) {
      stop(paste0("The input clean_pur_df is summed by chemical_class,\nand the ",
                  "input chemicals argument does not match any unique values ",
                  "of chemical_class."))
    }

    pur_df <- clean_pur_df %>% dplyr::filter(chemical_class == chemicals)

    } else {

    if (!toupper(chemicals) %in% unique(clean_pur_df$chemname)) {
      stop(paste0("The input chemicals argument does not match any unique ",
                  "values\nof active ingredients (the chemname column) in the ",
                  "input clean_pur_df data frame."))
    }

    pur_df <- clean_pur_df %>% dplyr::filter(chemname == toupper(chemicals))

    }

  if (!is.null(start_date)) {
    pur_df <- pur_df %>% dplyr::filter(date >= lubridate::ymd(start_date))
  }
  if (!is.null(end_date)) {
    pur_df <- pur_df %>% dplyr::filter(date >= lubridate::ymd(start_date))
  }

  if (section_township == "section") {
    pur_df2 <- pur_df %>%
      dplyr::filter(county_code == code) %>%
      dplyr::group_by(section) %>%
      dplyr::rename(pls = section) %>%
      dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE)) %>%
      dplyr::mutate(kg = ifelse(is.na(kg), 0, kg))
  } else {
    pur_df2 <- pur_df %>%
      dplyr::filter(county_code == code) %>%
      dplyr::group_by(township) %>%
      dplyr::rename(pls = township) %>%
      dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE)) %>%
      dplyr::mutate(kg = ifelse(is.na(kg), 0, kg))
  }

  if (color_by == "percentile") {

    perc <- as.data.frame(t(quantile(unique(pur_df2$kg),
                                     probs = percentile, na.rm = TRUE)))
    vec <- 0
    for (i in 1:length(percentile)) {
      vec <- c(vec, perc[, i])
    }
    vec <- c(vec, max(unique(pur_df2$kg), na.rm = TRUE))

    perc_numbers <- as.character(percentile * 100)
    first <- paste0("<=", perc_numbers[1], "th percentile")
    last <- paste0(">=", perc_numbers[length(perc_numbers)], "th")

    for (i in 1:(length(perc_numbers) - 1)) {
      label <- paste0(">=", perc_numbers[i], "th to <", perc_numbers[i+1], "th")
      if (i == 1) {
        middle <- label
      } else {
        middle <- c(middle, label)
      }
    }

    labels <- c(first, middle, last)

    pur_df3 <- pur_df2 %>%
      dplyr::mutate(category = as.character(cut(pur_df2$kg, vec, labels = labels)),
                    category = ifelse(is.na(category), "Missing", category))

    if ("Missing" %in% unique(pur_df3$category)) {
      pur_df3$category <- factor(pur_df3$category, levels = c(labels, "Missing"))
    } else {
      pur_df3$category <- factor(pur_df3$category, levels = labels)
    }

    viridis_discrete <- TRUE
    fill_var <- "category"

  } else {

    pur_df3 <- pur_df2
    viridis_discrete <- FALSE
    fill_var <- "kg"

  }

  if (section_township == "section") {
    county_df <- county_df %>% dplyr::rename(pls = MTRS)
  } else if (section_township == "township") {
    county_df <- county_df %>% dplyr::rename(pls = MTR)
  }

  pur_spatial <- pur_df3 %>% dplyr::left_join(county_df, by = "pls")

  long_range <- grDevices::extendrange(county_df$long)
  lat_range <- grDevices::extendrange(county_df$lat)

  suppressMessages(suppressWarnings(
    location <- ggmap::get_map(c(floor(county_bbox$min[1]), floor(county_bbox$min[2]),
                                 ceiling(county_bbox$max[1]), ceiling(county_bbox$max[2])),
                               color = "bw")))

  legend_label <- paste0("Applied Pesticides\n(kg/", section_township, ")")

  plot <- ggmap::ggmap(location) +
    ggplot2::geom_polygon(data = county_df, ggplot2::aes(x = long, y = lat, group = group),
                          color = "black", fill = NA) +
    ggplot2::geom_polygon(data = pur_spatial, ggplot2::aes_string(x = "long", y = "lat", ## aes_string
                                                        group = "group",
                                                        fill = fill_var)) +
    viridis::scale_fill_viridis(na.value = "white",
                                name = legend_label,
                                discrete = viridis_discrete,
                                option = fill_option) +
    ggplot2::theme_void() +
    ggplot2::coord_map(xlim = long_range, ylim = lat_range)

  if (crop) {
    long_range <- grDevices::extendrange(pur_spatial$long)
    lat_range <- grDevices::extendrange(pur_spatial$lat)
    plot <- plot + ggplot2::coord_map(xlim = long_range, ylim = lat_range)
  }

  if (color_by == "percentile") {
    percents <- sub("%", "", colnames(perc)) %>% as.numeric() / 100
    perc <- perc %>% tidyr::gather("percentile", "kg") %>%
      dplyr::mutate(percentile = percents)
    out <- list(plot = plot, data = pur_df3, percentile_values = perc)
  } else {
    out <- list(plot = plot, data = pur_df3)
  }

  return(out)

}

map_exposure <- function() {}

map_application <- function() {}
