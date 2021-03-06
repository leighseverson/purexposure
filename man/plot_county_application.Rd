% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-plot.R
\name{plot_county_application}
\alias{plot_county_application}
\title{Plot pesticide application by county.}
\usage{
plot_county_application(
  clean_pur_df,
  county = NULL,
  pls = NULL,
  color_by = "amount",
  percentile = c(0.25, 0.5, 0.75),
  start_date = NULL,
  end_date = NULL,
  chemicals = "all",
  fill = "viridis",
  crop = FALSE,
  alpha = 1,
  ggmap_background = TRUE,
  polygon_color = "grey",
  polygon_size = 0.25,
  ...
)
}
\arguments{
\item{clean_pur_df}{A data frame returned by \code{pull_clean_pur}.}

\item{county}{Optional. If your \code{clean_pur_df} data frame contains data
for multiple counties, this argument specifies which county you would like
to plot application for. Either a PUR county name or county code. California
names and county codes as they appear in PUR data sets can be found in the
county_codes data set available with this package.}

\item{pls}{Optional. Either "section" or "township". If your
\code{clean_pur_df} data frame has both a \code{section} and
\code{township} column, the \code{pls} argument specifies which pls unit
you would like to plot application for (the default in this case is
"section"). If you pulled data specifying \code{unit = "township"},
application will be plotted by township.}

\item{color_by}{Either "amount" (the default) or "percentile". Specifies
whether you would like application amounts to be colored according to
amount, resulting in a gradient legend, or by the percentile that they fall
into for the given data set and date range. You can specify percentile
cutpoints with the \code{percentile} argument.}

\item{percentile}{A numeric vector in (0, 1) specifying percentile cutpoints
if \code{color_by = "percentile"}. The default is \code{c(0.25, 0.5, 0.75)},
which results in four categories: < 25th percentile, >= 25th to < 50th,
>= 50th to < 75th, and >= 75th.}

\item{start_date}{Optional. "yyyy-mm-dd" giving a starting date for the date
range that you would like to map application for. The default is to plot
application for the entire date range in your \code{clean_pur_df} data frame.}

\item{end_date}{Optional. "yyyy-mm-dd" giving an ending date for the date
range that you would like to plot application for. The default is to plot
application for the entire date range in your \code{clean_pur_df} data frame.}

\item{chemicals}{Either "all" (the default) to plot summed active ingredients
present in your \code{clean_pur_df} data frame, a chemical class present in
the \code{chemical_class} column of the \code{clean_pur_df} data frame, or
a specific active ingredient present in the \code{chemname} column of the
\code{clean_pur_df} data frame.}

\item{fill}{A palette from the colormap package. The default is
"viridis". To see colormap palette options, visit
\url{https://bhaskarvk.github.io/colormap/} or run
\code{colormap::colormaps}.}

\item{crop}{TRUE / FALSE for whether you would like your plot zoomed in on
sections or townships with recorded application data.}

\item{alpha}{A number in [0,1] specifying the transparency of fill colors.
Numbers closer to 0 will result in more transparency. The default is 1.}

\item{ggmap_background}{TRUE / FALSE for whether you would like a ggmap
background.}

\item{polygon_color}{passed to the \code{color} argument of
\code{ggplot2::geom_polygon}.}

\item{polygon_size}{passed to the \code{size} argument of
\code{ggplot2::geom_polygon}.}

\item{...}{Used internally.}
}
\value{
A list with three elements:
\describe{
  \item{map}{A plot of the county with application summed by section or
  township and colored by amount or by percentile.}
  \item{data}{A data frame with the plotted application data.}
  \item{cutoff_values}{A data frame with two columns: \code{percentile}
  and \code{kg}, giving the cut points for each percentile in the
  \code{clean_pur_df} for the specified chemicals. This element of the list
  is not returned if \code{color_by = "amount"}.}
}
}
\description{
\code{plot_county_application} returns a plot of applied pesticides (either the
sum of all active ingredients present in the input \code{pull_clean_pur} data
frame, a specified chemical class, or a specified active ingredient). Application
is summed by section or township. PLS units can be shaded by amount or by
percentile.
}
\examples{
library(magrittr)
\donttest{
fresno_spdf <- readRDS(system.file("extdata", "fresno_spdf.rds",
                                   package = "purexposure"))
fresno_clean <- readRDS(system.file("extdata", "fresno_clean.rds",
                                    package = "purexposure"))
fresno_list <- fresno_clean \%>\% plot_county_application(spdf = fresno_spdf)}
\donttest{
# plot all active ingredients
fresno_df <- pull_clean_pur(2000:2001, "fresno")
fresno_list <- plot_county_application(fresno_df,
                                      color_by = "percentile",
                                      percentile = c(0.2, 0.4, 0.6, 0.8))
fresno_list$map
head(fresno_list$data)
fresno_list$cutoff_values

# plot a specific active ingredient
fresno_list2 <- plot_county_application(fresno_df, pls = "township",
                                       chemicals = "sulfur",
                                       fill = "plasma")
fresno_list2$map

# plot a chemical class
chemical_class_df <- purrr::map2_dfr(2010, c("methidathion", "parathion",
                                             "naled", "malathion",
                                             "trichlorfon"),
                                     find_chemical_codes) %>%
     dplyr::mutate(chemical_class = "organophosphates") %>%
     dplyr::select(-chemical)
op_yuba <- pull_clean_pur(2010, "yuba",
                          chemicals = chemical_class_df$chemname,
                          verbose = F, sum_application = T,
                          sum = "chemical_class",
                          chemical_class = chemical_class_df) %>%
   plot_county_application()
op_yuba$map
}

}
