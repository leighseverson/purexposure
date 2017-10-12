#' Pull California county SpatialPolygonsDataFrame
#'
#' \code{pull_spdf} pulls either the section or township-level
#' SpatialPolygonsDataFrame from a county's Geographic Information System (GIS)
#' shapefile.
#'
#' @inheritParams find_counties
#' @param section_township Either "section" (the default) or "township".
#'   Specifies whether you would like to pull a section- or township-level
#'   SpatialPolygonsDataFrame.
#' @param download_progress TRUE / FALSE indicating whether you would like a
#'   message and progress bar printed for the shapefile that is downloaded.
#'   The default value is FALSE.
#'
#' @return A SpatialPolygonsDataFrame object.
#'
#' @section Source:
#' SpatialPolygonDataFrame objects are downloaded from GIS shapefiles provided
#' by the California Department of Pesticide Regulation:
#' \url{http://www.cdpr.ca.gov/docs/emon/grndwtr/gis_shapefiles.htm}
#'
#' @examples
#' \dontrun{
#' trinity_shp <- pull_spdf("trinity", download_progress = TRUE)
#' plot(trinity_shp)
#'
#' del_norte_shp <- pull_spdf("08", "township", download_progress = TRUE)
#' plot(del_norte_shp)
#' }
#' @export
pull_spdf <- function(county, section_township = "section",
                           download_progress = FALSE) {

  if (county != "Statewide") {
    county_name <- find_counties(county, return = "names")
    county_name <- stringr::str_replace(county_name, " ", "_")

    shp_url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/grndwtr/",
                      county_name, "/", county_name, "_", section_township,
                      "s.zip")
    file <- paste0(county_name, "_", section_township, "s.zip")
  } else {
    shp_url <- "ftp://transfer.cdpr.ca.gov/pub/outgoing/grndwtr/Statewide/mtrnet.zip"
    file <- "mtrnet.zip"
  }

  current_dir <- getwd()
  temp_dir <- tempdir()
  setwd(temp_dir)

  invisible(suppressMessages(suppressWarnings(file.remove(list.files(temp_dir)))))

  if (download_progress) {
    quiet <- FALSE
  } else {
    quiet <- TRUE
  }

  download.file(shp_url, destfile = file, mode = "wb", quiet = quiet)

  unzip(file, exdir = temp_dir)

  shp_file <- list.files()[grepl(".shp", list.files()) &
                             !grepl(".xml", list.files())]

  shp <- rgdal::readOGR(shp_file,
                        layer = basename(strsplit(shp_file, "\\.")[[1]])[1],
                        verbose = FALSE)
  shp <- sp::spTransform(shp, sp::CRS("+init=epsg:4326"))

  setwd(current_dir)

  return(shp)

}

#' Convert county SpatialPolygonsDataFrame to a tidy data frame
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
#' df <- spdf_to_df(pull_spdf("fresno"))
#' df2 <- spdf_to_df(pull_spdf("sonoma"))
#'
#' # use view_county_df() function to easily plot the output data frames:
#' view_county_df(df)
#' view_county_df(df2)
#' }
#' @export
spdf_to_df <- function(spdf) {
  df <- suppressMessages(sp::merge(broom::tidy(spdf), as.data.frame(spdf),
                                   by.x = "id", by.y = 0))
  df <- df %>% dplyr::mutate(MTR = as.character(MTR))
  if ("MTRS" %in% colnames(df)) {
    df <- df %>% dplyr::mutate(MTRS = as.character(MTRS))
  }

  return(df)

}

#' Plot data frame spatial objects.
#'
#' \code{view_county_df} plots a data frame spatial object. Meant to be
#' analogous to the ease of using plot() to quickly view a
#' SpatialPolygonDataFrame object.
#'
#' @param df A data frame returned from the \code{spdf_to_df} function.
#'
#' @return A ggplot2 plot of the county.
#'
#' @examples
#' \dontrun{
#' shp <- pull_spdf("san diego", "township")
#' df <- spdf_to_df(shp)
#' view_county_df(df)
#' }
#' @export
view_county_df <- function(df) {
  plot <- ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat,
                                                  group = group)) +
    ggplot2::geom_polygon(color = "black", fill = NA) +
    ggplot2::theme_void()
  return(plot)
}
