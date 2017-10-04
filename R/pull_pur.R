#' Search for active ingredients.
#'
#' Returns a list of data frames with active ingredients (\code{chemname})
#' matching each search term given in the \code{chemicals} argument for a
#' particular year.
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
#' @examples
#' \dontrun{
#' chemical_search(2000, "methyl bromide")
#' chemical_search(1995, c("ammonia", "benzene"))
#' }
#' @importFrom dlyr %>%
#' @export
chemical_search <- function(year, chemicals) {

  suppressWarnings(suppressMessages(
    df <- readr::read_csv(file = paste0("~/Documents/pesticides_project/",
                                        "data-raw/PUR/", year, "/chemical.txt"))
    ))

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
