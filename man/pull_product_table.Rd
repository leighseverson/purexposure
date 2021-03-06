% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-pull.R
\name{pull_product_table}
\alias{pull_product_table}
\title{Pull PUR Product Table.}
\usage{
pull_product_table(years, quiet = FALSE)
}
\arguments{
\item{years}{A vector of four digit years in the range of 1990 to 2017.}

\item{quiet}{TRUE / FALSE indicating whether you would like a
message and progress bar printed for the product table that is downloaded.
The default value is FALSE.}
}
\value{
A data frame with four columns:
\describe{
\item{prodno}{Integer. The California Registration number for the pesticide
product. This corresponds to the \code{prodno} column in a raw or cleaned PUR
data set returned from \code{pull_raw_pur} or \code{pull_clean_pur}.}
\item{prodstat_ind}{Character. An indication of product registration status:
  \itemize{
  \item A = Active
  \item B = Inactive
  \item C = Inactive, Not Renewed
  \item D = Inactive, Voluntary Cancellation
  \item E = Inactive, Cancellation
  \item F = Inactive, Suspended
  \item G = Inactive, Invalid Data
  \item H = Active, Suspended}}
\item{product_name}{Character. The name of the product taken from the
registered product label. May have been modified by DPR's Registration Branch
to ensure uniqueness.}
\item{signlwrd_ind}{Integer. The signal word printed on the front of the
product label:
  \itemize{
  \item 1 = Danger (Poison)
  \item 2 = Danger (Only)
  \item 3 = Warning
  \item 4 = Caution
  \item 5 = None}}
\item{year}{Integer. Four digit year indicating the year for which data was
pulled.}
}
}
\description{
This function pulls a California Department of Pesticide Regulation Product
Table for a vector of years.
}
\details{
Product tables are pulled by year from the CDPR's FTP server. Downloaded
tables are saved in a temporary environment, which is deleted at the end of
the current R session.
}
\examples{
\donttest{
prod_95 <- pull_product_table(1995)}
}
