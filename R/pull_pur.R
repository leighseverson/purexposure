#' Search for active ingredients applied in a year.
#'
#' \code{chemical_search} returns a list of data frames with active ingredients
#' (\code{chemname}) matching each search term given in the \code{chemicals}
#' argument for a particular year.
#'
#' @param year A four-digit numeric year in the range of 1990 to 2015. Indicates
#'   the year in which you would like to search for active ingredients.
#' @param chemicals A string or vector of strings giving search terms of
#'   chemicals to match with active ingredients present in pesticides applied
#'   in the given year.
#'
#' @return A list of data frames (one data frame per search term). Each data
#'   frame has two columns: \code{chemname}, giving the active ingredient
#'   present in PUR records for that year, and \code{search_term}, giving the
#'   corresponding search term provided in the \code{chemicals} argument.
#'   Elements of the list are named according to given search terms.
#'
#' @section Note:
#' The PUR Chemical Lookup Table for a year lists all active ingredients present
#' in applied pesticides across the state of California. Therefore, PUR data for
#' a particular county may not include records for active ingredients returned
#' by \code{chemical_search} for the same year.
#'
#' @examples
#' \dontrun{
#' chemical_search(2000, "methyl bromide")
#' chemical_search(1995, c("ammonia", "benzene"))
#' }
#' @importFrom dlyr %>%
#' @export
chemical_search <- function(year, chemicals) {

  df <- get("chemical_list")
  year <- as.character(year)
  df <- df[[year]]

  chems_up <- toupper(chemicals)

  out <- list()

  for (i in 1:length(chems_up)) {
    df2 <- df[grep(chems_up[i], df$chemname), ]
    df2 <- df2 %>% dplyr::mutate(search_term = chemicals[i],
                                 chemname = as.factor(chemname)) %>%
      dplyr::select(chemname, search_term)
    name <- chemicals[i]
    out[[name]] <- df2
  }

  return(out)

}

#' Pull chemical codes from PUR Chemical Lookup Tables.
#'
#' For a vector of chemical names, \code{chemical_codes} returns
#' a data frame with corresponding chemical codes from the PUR Chemical Lookup
#' Table for a given year.
#'
#' @param year A four-digit numeric year in the range of 1990 to 2015. Indicates
#'   the year in which you would like to match chemical codes.
#' @inhereitParams A string or vector of strings giving search terms of
#'   chemicals to match with active ingredients present in pesticides applied
#'   in the given year. The default value is "all", which returns codes for all
#'   active ingredients applied in a given year.
#'
#' @return A data frame with three columns: \code{chemical}, with search terms
#'   given in the \code{chemicals} argument, \code{chemname}, with unique active
#'   ingredients corresponding to each search term, and \code{chem_code}, with
#'   chemical codes corresponding to each active ingredient. \code{chem_code}s
#'   are used to later filter raw PUR datasets.
#'
#' @section Note:
#' The PUR Chemical Lookup Table for a year lists all active ingredients present
#' in applied pesticides across the state of California. Therefore, PUR data for
#' a particular county may not include records for active ingredients returned
#' by \code{chemical_search} for the same year.
#'
#' @examples
#' \dontrun{
#' chemical_codes(2000, "methyl bromide")
#' chemical_search(1995, c("ammonia", "benzene"))
#' }
#' @importFrom dplyr %>%
#' @export
chemical_codes <- function(year, chemicals = "all") {

  df <- get("chemical_list")
  year <- as.character(year)
  df <- df[[year]]

  chems_up <- toupper(chemicals)

  if ("ALL" %in% chems_up) {
    out <- df
  } else {
    for (i in 1:length(chems_up)) {
      df2 <- df[grep(chems_up[i], df$chemname), ]
      df2 <- dplyr::mutate(df2, chemical = chems[i])
      if (i == 1) {
        out <- df2
      } else {
        out <- rbind(out, df2)
      }
    }
  }

  return(out)

}

























