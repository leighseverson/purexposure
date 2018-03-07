#' Plot a county's location in California.
#'
#' \code{plot_county_locations} returns one or multiple plots with county
#' locations in California given either a vector of county names or codes,
#' or a PUR data frame with a \code{county_cd}, \code{county_name},
#' \code{pur_code}, or \code{fips_code} column (A data frame returned from either
#' \code{pull_pur_file}, \code{pull_raw_pur}, or \code{pull_clean_pur}).
#'
#' @param counties_or_df A character vector of county names, pur codes, or fips
#'   codes. You can use the \code{county_codes} data set included with this
#'   package to check out PUR county names and codes. This argument can also be
#'   a data frame with a \code{county_cd}, \code{county_name}, \code{pur_code},
#'   or \code{fips_code} column. If you provide a data frame, a plot for every
#'   county with data in that data set will be output.
#' @param separate_plots TRUE / FALSE. If you provided multiple counties, whether you
#'   would like county outlines plotted in the same plot (FALSE), or if you would
#'   like separate plots returned in a list (TRUE). The default is FALSE.
#' @param fill_color A character string giving either a ggplot2 color or a
#'   hex color code ("#0000FF", for example). The default is "red".
#' @param alpha A number in [0,1] specifying the transparency of the fill
#'   color. Numbers closer to 0 will result in more transparency. The default is
#'   0.5.
#'
#' @return A ggplot or a list of ggplots of Califnornia with shaded-in counties.
#' List element names correspond to county names.
#'
#' @examples
#' \dontrun{
#' plot_county_locations("fresno")
#'
#' pur_df <- pull_clean_pur(1990, counties = c("01", "05", "12"))
#' plot_county_locations(pur_df)
#'
#' plot_list <- plot_county_locations(c("san bernardino", "ventura"),
#'                                    separate_plots = TRUE)
#' names(plot_list)
#' plot_list[[1]]
#' plot_list[[2]]
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @export
plot_county_locations <- function(counties_or_df, separate_plots = FALSE,
                                 fill_color = "red", alpha = 0.5) {

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
    check <- any(c("county_cd", "county_name", "pur_code", "fips_code") %in%
                     colnames(counties_or_df))
    if (!check) {
      stop(paste0("The counties_or_df data frame should have either a county_cd,",
                  " county_name, pur_code, or fips_code column.\nThis data",
                  " frame should be returned from either pull_pur_file(),",
                  " pull_raw_pur(), or pull_clean_pur()."))
    }
      county_col <- grep("county_cd", colnames(counties_or_df), value = TRUE)
      if (length(county_col) == 0) {
        county_col <- grep("county_name", colnames(counties_or_df), value = TRUE)
        if (length(county_col) == 0) {
          county_col <- grep("pur_code", colnames(counties_or_df), value = TRUE)
          if (length(county_col) == 0) {
            county_col <- grep("fips_code", colnames(counties_or_df), value = TRUE)
          }
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
                                       alpha)

  if (separate_plots) {

    if (length(counties) > 1) {
      out <- list()
      out[[1]] <- plot
      for (i in utils::tail(1:length(counties), -1)) {
        out[[i]] <- ca + ggplot2::geom_polygon(data = county_dfs[[i]],
                                               ggplot2::aes(x = long, y = lat, group = group),
                                               color = "transparent", fill = fill_color, alpha =
                                                 alpha)
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
                                             alpha)
        out <- plot

      }
    } else {
      out <- plot
    }

  }

  return(out)

}

#' Plot pesticide application by county.
#'
#' \code{plot_county_application} returns a plot of applied pesticides (either the
#' sum of all active ingredients present in the input \code{pull_clean_pur} data
#' frame, a specified chemical class, or a specified active ingredient). Application
#' is summed by section or township. PLS units can be shaded by amount or by
#' percentile.
#'
#' @param clean_pur_df A data frame returned by \code{pull_clean_pur}.
#' @param county Optional. If your \code{clean_pur_df} data frame contains data
#'   for multiple counties, this argument specifies which county you would like
#'   to plot application for. Either a PUR county name or county code. California
#'   names and county codes as they appear in PUR data sets can be found in the
#'   county_codes data set available with this package.
#' @param pls Optional. Either "section" or "township". If your
#'   \code{clean_pur_df} data frame has both a \code{section} and
#'   \code{township} column, the \code{pls} argument specifies which pls unit
#'   you would like to plot application for (the default in this case is
#'   "section"). If you pulled data specifying \code{unit = "township"},
#'   application will be plotted by township.
#' @param color_by Either "amount" (the default) or "percentile". Specifies
#'   whether you would like application amounts to be colored according to
#'   amount, resulting in a gradient legend, or by the percentile that they fall
#'   into for the given data set and date range. You can specify percentile
#'   cutpoints with the \code{percentile} argument.
#' @param percentile A numeric vector in (0, 1) specifying percentile cutpoints
#'   if \code{color_by = "percentile"}. The default is \code{c(0.25, 0.5, 0.75)},
#'   which results in four categories: < 25th percentile, >= 25th to < 50th,
#'   >= 50th to < 75th, and >= 75th.
#' @param start_date Optional. "yyyy-mm-dd" giving a starting date for the date
#'   range that you would like to map application for. The default is to plot
#'   application for the entire date range in your \code{clean_pur_df} data frame.
#' @param end_date Optional. "yyyy-mm-dd" giving an ending date for the date
#'   range that you would like to plot application for. The default is to plot
#'   application for the entire date range in your \code{clean_pur_df} data frame.
#' @param chemicals Either "all" (the default) to plot summed active ingredients
#'   present in your \code{clean_pur_df} data frame, a chemical class present in
#'   the \code{chemical_class} column of the \code{clean_pur_df} data frame, or
#'   a specific active ingredient present in the \code{chemname} column of the
#'   \code{clean_pur_df} data frame.
#' @param fill A palette from the colormap package. The default is
#'   "viridis". To see colormap palette options, visit
#'   \url{https://bhaskarvk.github.io/colormap/} or run
#'   \code{colormap::colormaps}.
#' @param crop TRUE / FALSE for whether you would like your plot zoomed in on
#'   sections or townships with recorded application data.
#' @param alpha A number in [0,1] specifying the transparency of fill colors.
#'   Numbers closer to 0 will result in more transparency. The default is 1.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{map}{A plot of the county with application summed by section or
#'   township and colored by amount or by percentile.}
#'   \item{data}{A data frame with the plotted application data.}
#'   \item{cutoff_values}{A data frame with two columns: \code{percentile}
#'   and \code{kg}, giving the cut points for each percentile in the
#'   \code{clean_pur_df} for the specified chemicals. This element of the list
#'   is not returned if \code{color_by = "amount"}.}
#' }
#'
#' @examples
#' \dontrun{
#' tulare_list <- pull_clean_pur(2010, "tulare") %>% plot_county_application()
#' names(tulare_list)
#'
#' # plot all active ingredients
#' fresno_df <- pull_clean_pur(2000:2001, "fresno")
#' fresno_list <- plot_county_application(fresno_df,
#'                                       color_by = "percentile",
#'                                       percentile = c(0.2, 0.4, 0.6, 0.8))
#' fresno_list$map
#' head(fresno_list$data)
#' fresno_list$cutoff_values
#'
#' # plot a specific active ingredient
#' fresno_list2 <- plot_county_application(fresno_df, pls = "township",
#'                                        chemicals = "sulfur",
#'                                        fill = "plasma")
#' fresno_list2$map
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
#'    plot_county_application()
#' op_yuba$map
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @export
plot_county_application <- function(clean_pur_df, county = NULL, pls = NULL,
                                   color_by = "amount",
                                   percentile = c(0.25, 0.5, 0.75),
                                   start_date = NULL, end_date = NULL,
                                   chemicals = "all", fill = "viridis",
                                   crop = FALSE, alpha = 1) {

  if (is.null(pls)) {
    if ("section" %in% colnames(clean_pur_df)) {
      section_township <- "section"
    } else {
      section_township <- "township"
    }
  } else {
    if ("section" %in% colnames(clean_pur_df)) {
      section_township <- tolower(pls)
    } else {
      section_township <- "township"
    }
  }

  if (!"pur_code" %in% colnames(clean_pur_df)) {
    stop(paste0("The clean_pur_df argument should be an unaltered data frame ",
                "returned from the pull_clean_pur() function."))
  }

  # pull county shapefile
  if (is.null(county)) {
    code <- unique(clean_pur_df$pur_code)
    if (length(code) > 1) {
      counties <- paste(find_counties(code, "names"), collapse = ", ")
      stop(paste0("Your clan_pur_df data frame has data for more than one ",
                  "county (", counties, "). You can specify which county to ",
                  "plot data for with the county argument."))
    }
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

  } else if ("chemical_class" %in% colnames(clean_pur_df)) {

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
      dplyr::filter(pur_code == code) %>%
      dplyr::group_by(section) %>%
      dplyr::rename(pls = section) %>%
      dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE)) %>%
      dplyr::mutate(kg = ifelse(is.na(kg), 0, kg))
  } else {
    pur_df2 <- pur_df %>%
      dplyr::filter(pur_code == code) %>%
      dplyr::group_by(township) %>%
      dplyr::rename(pls = township) %>%
      dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE)) %>%
      dplyr::mutate(kg = ifelse(is.na(kg), 0, kg))
  }

  if (color_by == "percentile") {

    cutpoints_list <- help_categorize(section_data = pur_df2, buffer_or_county = "buffer",
                                     percentile = percentile) # find cutpoints based on
                                                              # given data frame
    pur_df3 <- cutpoints_list$df
    labels <- cutpoints_list$categories
    cutoff_values <- cutpoints_list$cutoff_values

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

  colnames(pur_df3)[1] <- "pls"
  pur_spatial <- pur_df3 %>% dplyr::left_join(county_df, by = "pls")

  long_range <- grDevices::extendrange(county_df$long)
  lat_range <- grDevices::extendrange(county_df$lat)

  suppressMessages(suppressWarnings(
    location <- ggmap::get_map(c(floor(county_bbox$min[1]), floor(county_bbox$min[2]),
                                 ceiling(county_bbox$max[1]), ceiling(county_bbox$max[2])),
                               color = "bw")))

  legend_label <- paste0("Applied Pesticides\n(kg/", section_township, ")")

  colormaps_vec <- unlist(colormap::colormaps)
  names(colormaps_vec) <- NULL

  if (!fill %in% colormaps_vec) {
    stop(paste0("The fill argument should be a color palette from the ",
                "colormap package."))
  }

  gradient <- colormap::colormap(fill, nshades = 1000, alpha = alpha)
  # gradient <- c("#FFFFFF", gradient)

  plot <- ggmap::ggmap(location) +
    ggplot2::geom_polygon(data = county_df, ggplot2::aes(x = long, y = lat, group = group),
                          color = "black", fill = NA) +
    ggplot2::geom_polygon(data = pur_spatial, ggplot2::aes_string(x = "long", y = "lat", ## aes_string
                                                                  group = "group",
                                                                  fill = fill_var))

  if (color_by == "amount") {

    plot <- plot +
      scale_fill_gradientn2(colours = gradient, alpha = alpha, name = legend_label,
                            na.value = "#FFFFFF")

  } else if (color_by == "percentile") {

    categories <- as.character(levels(pur_spatial$category))

    if (!"None recorded" %in% categories) {
      categories <- c(categories, "missing")
    }

    n_cols <- as.integer(length(gradient)/(length(categories)-1))
    end_i <- length(categories) - 1

    for (i in 1:end_i) {
      col_vec <- gradient[n_cols*i]
      if (i == 1) {
        cols_out <- col_vec
      } else {
        cols_out <- c(cols_out, col_vec)
      }
    }

    cols_out <- c(cols_out, "#FFFFFF")
    names(cols_out) <- categories

    plot <- plot  +
      ggplot2::scale_fill_manual(values = cols_out, name = legend_label)

  }

  plot <- plot +
    ggplot2::theme_void() +
    ggplot2::coord_map(xlim = long_range, ylim = lat_range)

  if (crop) {
    long_range <- grDevices::extendrange(pur_spatial$long)
    lat_range <- grDevices::extendrange(pur_spatial$lat)
    plot <- plot + ggplot2::coord_map(xlim = long_range, ylim = lat_range)
  }

  if (color_by == "percentile") {
    out <- list(map = plot, data = pur_df3, cutoff_values = cutoff_values)
  } else {
    out <- list(map = plot, data = pur_df3)
  }

  return(out)

}

#' Plot exposure to applied pesticides at a location.
#'
#' \code{plot_exposure} returns a plot of pesticide application in the PLS units
#' intersected by a buffer for each combination of time period, applied active
#' ingredients, and application method relevant for the exposure values returned
#' from \code{calculate_exposure}.
#'
#' @inheritParams plot_county_application
#' @param exposure_list A list returned from \code{calculate_exposure}.
#' @param buffer_or_county Either "county" (the default) or "buffer". Specifies
#'   whether you would like colors to be scaled according to the limits
#'   of application within the buffer, or in the county for the same time period,
#'   chemicals, and method of application.
#' @param pls_labels TRUE / FALSE for whether you would like sections or townships
#'   to be labeled with their PLS ID. The default is \code{FALSE}.
#' @param pls_labels_size A number specifying the size of PLS labels. The default
#'   is 4.
#' @param alpha A number in [0,1] specifying the transparency of fill colors.
#'   Numbers closer to 0 will result in more transparency. The default is 0.7.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{maps}{A list of plots. One plot for each exposure value returned in
#'   the \code{exposure} element of the \code{calculate_exposure} list.}
#'   \item{pls_data}{A list of data frames with 12 columns: \code{pls}, giving
#'   the PLS ID, \code{percent}, the % intersection of that PLS unit with the
#'   buffer, \code{kg}, the amount of kg of pesticides applied in that PLS unit
#'   for the relevant time period, chemicals, and application method,
#'   \code{kg_intersection}, \code{kg} multiplied by \code{percent} (this is the
#'   value that is plotted), \code{start_date}, \code{end_date}, \code{chemicals},
#'   \code{aerial_ground}, which give the time period, chemicals, and application
#'   method for each plot/exposure estimate, \code{none_recorded}, \code{location},
#'   \code{radius} (m), and \code{area} (m^2).}
#'   \item{cutoff_values}{A list of data frames with two columns: \code{percentile} and
#'   \code{kg} giving the cutoff values for each percentile. Only returned if
#'   \code{color_by = "percentile"}.}
#' }
#'
#' @examples
#' \dontrun{
#' tulare_list <- pull_clean_pur(2010, "tulare") %>%
#'    calculate_exposure(location = "-119.3473, 36.2077", radius = 3500) %>%
#'    plot_exposure()
#' names(tulare_list)
#' tulare_list$maps
#' tulare_list$pls_data
#' tulare_list$exposure
#'
#' # return one plot, pls_data data frame, exposure row, and cutoff_values
#' # data frame for each exposure combination
#'
#' dalton_list <- pull_clean_pur(2000, "modoc") %>%
#'     calculate_exposure(location = "-121.4182, 41.9370",
#'                        radius = 4000,
#'                        time_period = "6 months",
#'                        aerial_ground = TRUE) %>%
#'     plot_exposure(fill = "plasma")
#' do.call("rbind", dalton_list$exposure)
#' # one map for each exposure value (unique combination of chemicals,
#' # dates, and aerial/ground application)
#' dalton_list$maps[[1]]
#' dalton_list$maps[[2]]
#' dalton_list$maps[[3]]
#' dalton_list$maps[[4]]
#' dalton_list$maps[[5]]
#' dalton_list$maps[[6]]
#'
#' # exposure to a particular active ingredient
#' # plot percentile categories instead of amounts
#' chemical_df <- rbind(find_chemical_codes(2009, c("metam-sodium"))) %>%
#'      dplyr::rename(chemical_class = chemical)
#'
#' santa_maria <- pull_clean_pur(2008:2010, "santa barbara",
#'                               chemicals = chemical_df$chemname,
#'                               sum_application = TRUE,
#'                               sum = "chemical_class",
#'                               chemical_class = chemical_df) %>%
#'      calculate_exposure(location = "-119.6122, 34.90635",
#'                         radius = 3000,
#'                         time_period = "1 year",
#'                         chemicals = "chemical_class") %>%
#'      plot_exposure(color_by = "percentile")
#' do.call("rbind", santa_maria$exposure)
#' santa_maria$maps[[1]]
#' santa_maria$maps[[2]]
#' santa_maria$maps[[3]]
#'
#' # scale colors based on buffer or county
#' clotho <- pull_clean_pur(1996, "fresno") %>%
#'   dplyr::filter(chemname == "SULFUR") %>%
#'   calculate_exposure(location = "-119.6082, 36.7212",
#'                     radius = 1500)
#'
#' plot_exposure(clotho, "amount", buffer_or_county = "county", pls_labels = TRUE)$maps
#' plot_exposure(clotho, "amount", buffer_or_county = "buffer", pls_labels = TRUE)$maps
#' }
#' @importFrom magrittr %>%
#' @export
plot_exposure <- function(exposure_list, color_by = "amount",
                          buffer_or_county = "county",
                          percentile = c(0.25, 0.5, 0.75), fill = "viridis",
                          alpha = 0.7, pls_labels = FALSE, pls_labels_size = 4) {

  buffer_df <- exposure_list$buffer_plot_df

  clean_pur <- exposure_list$clean_pur_df

  pls_data <- exposure_list$meta_data %>%
    dplyr::group_by(start_date, end_date, aerial_ground, chemicals) %>%
    tidyr::nest()

  # each $data row is input into function below to return a plot.

  colormaps_vec <- unlist(colormap::colormaps)
  names(colormaps_vec) <- NULL

  if (!fill %in% colormaps_vec) {
    stop(paste0("The fill argument should be a color palette from the ",
                "colormap package."))
  }

  gradient <- colormap::colormap(fill, nshades = 1000, alpha = alpha)

  location_longitude <- unique(exposure_list$exposure$longitude)
  location_latitude <- unique(exposure_list$exposure$latitude)

  buffer2 <- buffer_df %>%
    dplyr::filter(id == "buffer1")

  buffer_df <- buffer_df %>%
    tidyr::gather(key = "section_or_township", value = "pls", MTR, MTRS)

  buffer <- dplyr::select(buffer2, long, lat)
  buffer <- buffer[grDevices::chull(buffer), ]
  buffer <- methods::as(buffer, "gpc.poly")

  # want pls_data in same order as exposure_list$exposure

  pls_data <- pls_data %>% dplyr::mutate(aerial_ground = as.character(aerial_ground),
                                         chemicals = as.character(chemicals))

  pls_data <- exposure_list$exposure %>%
    dplyr::select(start_date, end_date, chemicals, aerial_ground) %>%
    dplyr::full_join(pls_data, by = c("start_date", "end_date", "aerial_ground",
                                      "chemicals")) %>%
    dplyr::ungroup() %>%
    dplyr::rename(data_pls = data) %>%
    dplyr::mutate(none_recorded = NA)

  for (i in 1:nrow(pls_data)) {
    data_pls_df <- pls_data$data_pls[[i]]
    if (all(data_pls_df$none_recorded == TRUE)) {
      pls_data$none_recorded[i] <- TRUE
    } else {
      pls_data$none_recorded[i] <- FALSE
    }

    pls_data$data_pls[[i]] <- pls_data$data_pls[[i]] %>%
      dplyr::mutate(kg = ifelse(kg == 0, NA, kg),
                    kg_intersection = ifelse(kg_intersection == 0, NA, kg))

  }

  pls_data <- pls_data %>% dplyr::select(1:4, 6, 5)

  out_maps <- list()
  for (i in 1:nrow(pls_data)) {
    map <- help_map_exp(pls_data$start_date[i], pls_data$end_date[i],
                        pls_data$chemicals[i], pls_data$aerial_ground[i],
                        pls_data$none_recorded[i], pls_data$data_pls[[i]],
                        gradient, location_longitude, location_latitude,
                        buffer_df, buffer2, buffer, buffer_or_county, alpha,
                        clean_pur, pls_labels, pls_labels_size, percentile,
                        color_by)
    out_maps[[i]] <- map
  }

  # reformat list
  plots <- list()
  dfs <- list()
  cutoff_values <- list()
  exposures <- list()
  for (i in 1:length(out_maps)) {
    plots[[i]] <- out_maps[[i]]$plot
    dfs[[i]] <- out_maps[[i]]$data
    cutoff_values[[i]] <- out_maps[[i]]$cutoff_values
    exposures[[i]] <- exposure_list$exposure[i,]
  }

  if (color_by == "amount") {
    out_maps_list <- list(maps = plots, pls_data = dfs,
                          exposure = exposures)
  } else if (color_by == "percentile") {
    out_maps_list <- list(maps = plots, pls_data = dfs,
                          exposure = exposures,
                          cutoff_values = cutoff_values)
  }

  return(out_maps_list)
}

#' Plot time series of active ingredients in applied pesticides.
#'
#' \code{plot_application_timeseries} returns a \code{ggplot2} time series plot
#' of pesticides present in a \code{pull_clean_pur} data frame. You can choose
#' whether to facet the time series by active ingredient (\code{chemname}) or by
#' \code{chemical_class}.
#'
#' @param clean_pur_df A data frame returned from \code{pull_clean_pur}.
#' @param facet TRUE / FALSE for whether you would like time series
#'   plots to be faceted by unique \code{chemname} or \code{chemical_class}
#'   column values. If \code{facet = FALSE} (the default), all active ingredients
#'   present in the data set will be summed per day.
#' @param axes A character string passed on to the \code{scales} argument of
#'   \code{ggplot2::facet_wrap} (\code{"fixed"}, \code{"free"}, \code{"free_x"},
#'   or \code{"free_y"}). The default is \code{"fixed"}.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' \dontrun{
#' pull_clean_pur(1990:1992, "fresno") %>%
#'     dplyr::filter(chemname %in% toupper(c("methyl bromide", "sulfur"))) %>%
#'     plot_application_timeseries(facet = TRUE)
#'
#' pull_clean_pur(2000, "riverside") %>% plot_application_timeseries()
#' }
#' @export
plot_application_timeseries <- function(clean_pur_df, facet = FALSE,
                                        axes = "fixed") {

  if (facet) {
    if ("chemname" %in% colnames(clean_pur_df)) {
      plot <- clean_pur_df %>%
        dplyr::group_by(date, chemname)
    } else if ("chemical_class" %in% colnames(clean_pur_df)) {
      plot <- clean_pur_df %>%
        dplyr::group_by(date, chemical_class)
    }
  } else {
    plot <- clean_pur_df %>%
      dplyr::group_by(date)
  }


  plot <- plot %>%
    dplyr::summarise(kg_perday = sum(kg_chm_used, na.rm = TRUE)) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = kg_perday)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Application date",
                  y = "Kilograms of active ingredient\napplied per day ")

  if (facet) {
    if ("chemname" %in% colnames(clean_pur_df)) {
      plot <- plot +
        ggplot2::facet_wrap(~chemname, scales = axes)
    } else if ("chemical_class" %in% colnames(clean_pur_df)) {
      plot <- plot +
        ggplot2::facet_wrap(~chemical_class, scales = axes )
    }
  }

  return(plot)

}

#' Plot exposure for multiple locations in a county.
#'
#' \code{plot_locations_exposure} returns a plot of exposure to applied pesticides
#' for multiple locations in a given county.
#'
#' @param exposure_df A data frame returned from \code{write_exposure} with 10
#'   columns, including \code{exposure}, \code{location}, \code{radius},
#'   \code{longitude}, and \code{latitude}. This data frame should be filtered
#'   so that there is one exposure value per location.
#' @param section_township Either "section" (the default) or "township". Specifies
#'   which PLS unit to plot the county by.
#' @inheritParams plot_county_application
#'
#' @return A plot with one point per location, colored by each location's
#' corresponding exposure value.
#'
#' @examples
#' \dontrun{
#' fresno <- pull_clean_pur(2000, "fresno")
#' df <- data.frame(location = c("295 West Saginaw Ave., Caruthers, CA 93609",
#'                               "55190 Point Rd., Big Creek, CA 93605"),
#'                  start_date = "2000-01-01", end_date = "2000-12-31")
#' write_exposure(fresno, df, 3000, "~/Documents/fresno")
#' exp_df <- readRDS("~/Documents/fresno/exposure_df.rds")
#' plot_locations_exposure(exp_df)
#'
#' # this exposure_df was saved from the example for write_exposure()
#' exp_df2 <- readRDS("~/Documents/fresno_schools/exposure_df.rds")
#' # filter to one exposure value per location
#' exp_df2 <- dplyr::filter(exp_df2, chemicals == "methyl bromide" &
#'                           radius == 3000)[c(1, 3, 5),]
#' plot_locations_exposure(exp_df2)
#' }
#' @importFrom magrittr %>%
#' @export
plot_locations_exposure <- function(exposure_df, section_township = "section",
                                    fill = "viridis", alpha = 1) {

  check <- nrow(exposure_df) == length(unique(exposure_df$location))
  if (!check) {
    stop(paste0("Filter your exposure_df so that there is one row per location ",
                "in your exposure_df."))
  }

  for (i in 1:nrow(exposure_df)) {

    buffer <- help_calculate_buffers(exposure_df[i,])
    if (i == 1) {
      buffer_out <- buffer
    } else {
      buffer_out <- dplyr::bind_rows(buffer_out, buffer)
    }

  }

  buffer_out <- buffer_out %>%
    dplyr::full_join(dplyr::select(exposure_df, location, exposure), by = "location")

  county <- find_location_county(exposure_df[1,]$location)$county

  shp <- pull_spdf(county, section_township)
  df <- spdf_to_df(shp)

  colormaps_vec <- unlist(colormap::colormaps)
  names(colormaps_vec) <- NULL

  gradient <- colormap::colormap(fill, nshades = 1000, alpha = alpha)

  if (!fill %in% colormaps_vec) {
    stop(paste0("The fill argument should be a color palette from the ",
                "colormap package."))
  }

  plot <- ggplot2::ggplot(data = df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group),
                         color = "lightgrey", fill = NA) +
    ggplot2::geom_polygon(data = buffer_out, ggplot2::aes(x = long, y = lat,
                                                          group = location,
                                                          fill = exposure)) +
    ggplot2::coord_map() +
    ggplot2::theme_void() +
    scale_fill_gradientn2(colours = gradient, alpha = alpha,
                          name = expression(paste("Exposure (", "kg/", "m"^{2}, ")")))

  return(plot)

}
