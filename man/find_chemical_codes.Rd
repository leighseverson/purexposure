% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-find.R
\name{find_chemical_codes}
\alias{find_chemical_codes}
\title{Pull active ingredient chemical codes from PUR Chemical Lookup Tables.}
\usage{
find_chemical_codes(years, chemicals = "all", by_year = FALSE)
}
\arguments{
\item{years}{A vector of four-digit numeric years, starting with 1990.
Indicates the years in which you would like to match chemical codes.}

\item{chemicals}{A string or vector of strings giving search terms of
chemicals to match with active ingredients present in pesticides applied
in the given year. The default value is "all", which returns codes for all
active ingredients applied in a given year.}

\item{by_year}{TRUE / FALSE for whether you would like PUR Chemical Lookup
Tables separated by year (in a `year` column). If `by_year` is `FALSE`, the
default, a data frame is returned with unique results from all years given
in the `years` argument.}
}
\value{
A data frame:
  \describe{
    \item{chem_code}{An integer value with chemical codes corresponding to
    each active ingredient. \code{chem_code} values are used to later filter
    raw PUR data sets.}
    \item{chemname}{A character string giving unique active ingredients
    corresponding to each search term.}
    \item{chemical}{A character string with search terms given in the
    \code{chemicals} argument. Not included if the `chemicals` argument is
    set to its default value of "all".}
    \item{year}{Included if `by_year` is set to `TRUE`.}
}
}
\description{
For a vector of chemical names, \code{find_chemical_codes} returns
a data frame with corresponding chemical codes from the PUR Chemical Lookup
Table for a given year. This function uses pattern matching to return results.
As a starting place, or for more thorough classifications, see the CA
Department of Pesticide Regulation's Summary of Pesticide Use Report Data,
Indexed by Chemical (2016):
\url{https://www.cdpr.ca.gov/docs/pur/pur16rep/chmrpt16.pdf}
}
\section{Note}{

The PUR Chemical Lookup Table for a year lists all active ingredients present
in applied pesticides across the state of California. Therefore, PUR data for
a particular county may not include records for active ingredients returned
by \code{find_chemical_codes} for the same year.
}

\examples{
find_chemical_codes(2000, "methyl bromide")
find_chemical_codes(1995, c("ammonia", "benzene"))
}
