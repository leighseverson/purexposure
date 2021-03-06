% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-pull.R
\name{pull_clean_pur}
\alias{pull_clean_pur}
\title{Pull cleaned PUR data by counties, years, and active ingredients.}
\usage{
pull_clean_pur(
  years = "all",
  counties = "all",
  chemicals = "all",
  sum_application = FALSE,
  unit = "section",
  sum = "all",
  chemical_class = NULL,
  aerial_ground = TRUE,
  verbose = TRUE,
  quiet = FALSE,
  ...
)
}
\arguments{
\item{years}{A four-digit numeric year or vector of years, starting with
1990. Indicates the years for which you would like to pull PUR
data sets. \code{years == "all"} will pull data from 1990 through the
most recent year of available data.}

\item{counties}{A vector of character strings giving either a county name,
two digit PUR county codes, or six-digit FIPS county codes for each county.
Not case sensitive. California names, county codes as they appear in PUR
data sets, and FIPS county codes can be found in the \code{county_codes}
data set available with this package. For example, to return data for
Alameda county, enter either "alameda", "01", or "06001" for the
\code{counties} argument. \code{counties = "all"} will return data for all
58 California counties (this will take a while to run).}

\item{chemicals}{A string or vector of strings giving search terms of
chemicals to match with active ingredients present in pesticides applied in
the given years. The default value is "all", which returns records for all
active ingredients applied in a given year. See the CDPR's Summary of PUR
Data document here:
\url{http://www.cdpr.ca.gov/docs/pur/pur08rep/chmrpt08.pdf} for
comprehensive classifications of active ingredients.}

\item{sum_application}{TRUE / FALSE indicating if you would like to sum the
amounts of applied active ingredients by day, the geographic unit
given in \code{unit}, and by either active ingredients or chemical class
(indicated by \code{sum} and \code{chemical_class}). The default value
is FALSE.}

\item{unit}{A character string giving either "section" or "township".
Specifies whether applications of each active ingredient should be summed
by California section (the default) or by township. Only used if
\code{sum_application} is \code{TRUE}.}

\item{sum}{A character string giving either "all" (the
default) or "chemical_class". If \code{sum_application = TRUE},
\code{sum} indicates whether you would like to sum across all active
ingredients, giving an estimation of the total pesticides applied in a
given section or township ("all"), or by a chemical class specified in a
data frame given in the argument \code{chemical_class}.}

\item{chemical_class}{A data frame with only three columns: \code{chem_code},
\code{chemname}, and \code{chemical_class}. \code{chem_code} should have
integer values giving PUR chemical codes, and \code{chemname} should have
character strings with corresponding PUR chemical names (these can be
searched for using the \code{find_chemical_codes} function or with the
\code{chemical_list} data set included with this package). The
\code{chemical_class} column should have character strings indicating the
chemical class corresponding to each \code{chem_code}. The
\code{chemical_class} for a group of active ingredients should be decided
upon by the user. Only used if \code{sum = "chemical_class"}. See
the CDPR's Summary of PUR Data document here:
\url{http://www.cdpr.ca.gov/docs/pur/pur08rep/chmrpt08.pdf} for
comprehensive classifications of active ingredients.}

\item{aerial_ground}{TRUE / FALSE indicating if you would like to
retain aerial/ground application data ("A" = aerial, "G" = ground, and
"O" = other.) The default is TRUE.}

\item{verbose}{TRUE / FALSE indicating whether you would like a single message
printed indicating which counties and years you are pulling data for. The
default value is TRUE.}

\item{quiet}{TRUE / FALSE indicating whether you would like a
message and progress bar printed for each year of PUR data that
is downloaded. The default value is FALSE.}

\item{...}{Used internally.}
}
\value{
A data frame:
  \describe{
    \item{chem_code}{An integer value giving the PUR chemical code
    for the active ingredient applied. Not included if
    \code{sum_application = TRUE} and \code{sum = "chemical_class"}.}
    \item{chemname}{A character string giving PUR chemical active
    ingredient names. Unique values of \code{chemname} are matched with terms
    provided in the \code{chemicals} argument. Not included
    if \code{sum_application = TRUE} and \code{sum = "chemical_class"}.}
    \item{chemical_class}{If \code{sum_application = TRUE} and
    \code{sum = "chemical_class"}, this column will give values of the
    \code{chemical_class} column in the input \code{chemical_class} data frame.
    If there are active ingredients pulled based on the
    \code{chemicals} argument that are not present in the \code{chemical_class}
    data frame, these chemicals will be summed under the class "other".}
    \item{kg_chm_used}{A numeric value giving the amount of the active
    ingredient applied (kilograms).}
    \item{section}{A string nine characters long indicating the section
    of application. PLS sections are uniquely identified by a combination of
    base line meridian (S, M, or H), township (01-48), township direction
    (N or S), range (01-47), range direction (E or W) and section number
    (01-36). This column is not included if
    \code{sum_application = TRUE} and \code{unit = "township"}.}
    \item{township}{A string seven characters long indicating the township
    of application. PLS townships are uniquely identified by a combination of
    base line meridian (S, M, or H), township (01-48), township direction
    (N or S), range (01-47), and range direction (E or W).}
    \item{county_name}{A character string giving the county name where
    application took place.}
    \item{pur_code}{A string two characters long giving the PUR county
    code where application took place.}
    \item{fips_code}{A string six characters long giving the FIPS county
    code where application took place.}
    \item{date}{The date of application (yyyy-mm-dd).}
    \item{aerial_ground}{A character giving the application method.
    "A" = aerial, "G" = ground, and "O" = other. Not included
    if \code{aerial_ground = FALSE}.}
    \item{use_no}{A character string identifying unique application of an
    active ingredient across years. This value is a combination of the raw PUR
    \code{use_no} column and the year of application. Not included if
    \code{sum_appliction = TRUE}.}
    \item{outlier}{If the amount listed in \code{kg_chm_used} has been
    corrected for large amounts entered in error, this column lists the raw
    value of recorded kilograms of applied chemicals. Otherwise \code{NA}. The
    algorithm for identifying and replacing outliers was developed based on
    methods used by Gunier et al. (2001). Please see the package vignette for
    more detail regarding these methods. Not included if
    \code{sum_application = TRUE}.}
    \item{prodno}{Integer. The California Registration Number for the applied
    pesticide (will be repeated for different active ingredients present in
    the product). You can match product registration numbers with product
    names, which can be pulled using the \code{pull_product_table} function.
    This column is not returned if \code{sum_application = TRUE}.}
 }
}
\description{
\code{pull_clean_pur} returns a data frame of cleaned Pesticide Use Report data
filtered by counties, years, and active ingredients. Active ingredients
or chemical classes present in applied pesticides can be summed by either
Public Land Survey (PLS) section or township.
}
\details{
PUR data sets are pulled by county from the CDPR's FTP server. Downloaded
PUR data sets are saved in a temporary environment, which is deleted at the
end of the current R session.
}
\section{Note}{

  \itemize{
    \item The \code{chemical_list} data frame for a particular year lists
    active ingredients present in applied pesticides across the state of
    California. Therefore, PUR data for a particular county may not include
    records for active ingredients listed in the \code{chemical_list} data set
    for the same year.
    \item To pull raw PUR data, see the \code{pull_raw_pur} function.
    For documentation of raw PUR data, see the Pesticide Use Report Data User
    Guide & Documentation document published by the California Department of
    Pesticide Regulation. This file is saved as "cd_doc.pdf" in
    any "pur[year].zip" file between 1990 and 2017 found here:
    \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/}.
    \item{If this function returns an error (because the FTP site is down,
    for example), check your working directory. You may need to change it
    back from a temporary directory.}
}
}

\examples{
library(magrittr)
\dontshow{
fresno_raw <- readRDS(system.file("extdata", "fresno_raw.rds",
                                  package = "purexposure"))
df <- pull_clean_pur(2000, "fresno", raw_pur = fresno_raw)}
\donttest{
df <- pull_clean_pur(years = 2000:2001,
                     counties = c("06001", "29", "riverside"),
                     chemicals = "methylene",
                     aerial_ground = TRUE)

# filter to active ingredients present in particular products
prod_nos <- find_product_name(2003, "insecticide") %>%
    dplyr::select(prodno) %>%
    tibble_to_vector()

df2 <- pull_clean_pur(2003, "10") %>%
    dplyr::filter(prodno %in% prod_nos)

# Sum application by active ingredients
df3 <- pull_clean_pur(years = 2009:2010,
                      counties = c("01", "29", "riverside"),
                      unit = "township",
                      sum_application = TRUE)

# Or by chemical classes
chemical_class_df <- rbind(find_chemical_codes(2000, "methylene"),
                           find_chemical_codes(2000, "aldehyde")) %>%
   dplyr::rename(chemical_class = chemical)

df4 <- pull_clean_pur(years = 1995,
                      counties = "fresno",
                      chemicals = chemical_class_df$chemname,
                      sum_application = TRUE,
                      sum = "chemical_class",
                      unit = "township",
                      chemical_class = chemical_class_df)
}
}
