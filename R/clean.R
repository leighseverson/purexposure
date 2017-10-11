#' Pull cleaned PUR data by counties, years, and active ingredients
#'
#' \code{clean_pur_data} returns a data frame of cleaned Pesticide Use Report data
#' filtered by counties, years, and active ingredients. Active ingredients
#' or chemical classes present in applied pesticides can be summed by either
#' Public Land Survey (PLS) section or township.
#'
#' @inheritParams pull_raw_pur
#' @param chemicals A string or vector of strings giving search terms of
#'   chemicals to match with active ingredients present in pesticides applied in
#'   the given years. The default value is "all", which returns records for all
#'   active ingredients applied in a given year.
#' @param sum_application TRUE / FALSE indicating if you would like to sum the
#'   amounts of applied active ingredients by day, the geographic unit
#'   given in \code{unit}, and by either active ingredients or chemical class
#'   (indicated by \code{sum_by} and \code{chemical_class}). The default value
#'   is FALSE.
#' @param unit A character string giving either "section" or "township".
#'   Specifies whether applications of each active ingredient should be summed
#'   by California section (the default) or by township. Only used if
#'   \code{sum_application} is \code{TRUE}.
#' @param sum_by A character string giving either "active_ingredient" (the
#'   default) or "chemical_class". If \code{sum_application = TRUE},
#'   \code{sum_by} indicates whether you would like to sum by each active
#'   ingredient, or by a chemical class specified in a data frame given in the
#'   argument \code{chemical_class}.
#' @param chemical_class A data frame with three columns: \code{chem_code},
#'   \code{chemname}, and \code{chemical_class}. \code{chem_code} should have
#'   integer values giving PUR chemical codes, and \code{chemname} should have
#'   character strings with corresponding PUR chemical names (these can be
#'   searched for using the \code{find_chemical_codes} function or with the
#'   \code{chemical_list} dataset included with this package). The
#'   \code{chemical_class} column should have character strings indicating the
#'   chemical class corresponding to each \code{chem_code}. The
#'   \code{chemical_class} for a group of active ingredients should be decided
#'   upon by the user. Only used if \code{sum_by = "chemical_class"}. Please see
#'   the package vignette for more detail regarding this option.
#' @param include_ag TRUE / FALSE indicating if you would like to retain
#'   aerial/ground application data when summing application across sections or
#'   townships and chemicals. The default is TRUE. \code{include_ag = TRUE}
#'   could result in more than one record per section, date, and active
#'   ingredient or chemical class if \code{aerial_ground} application differs
#'   for the same chemical in the same section on the same day.
#'
#' @return A data frame:
#'   \describe{
#'     \item{chem_code}{An integer value giving the PUR chemical code
#'     for the active ingredient applied. Not included if
#'     \code{sum_application = TRUE} and \code{sum_by = "chemical_class"}.}
#'     \item{chemname}{A character string giving PUR chemical active
#'     ingredient names. Unique values of \code{chemname} are matched with terms
#'     provided in the \code{chemicals} argument. Not included if
#'     \code{sum_application = TRUE} and \code{sum_by = "chemical_class"}.}
#'     \item{chemical_class}{If \code{sum_application = TRUE} and
#'     \code{sum_by = "chemical_class"}, this column will give values of the
#'     \code{chemical_class} column in the input \code{chemical_class} data frame.
#'     If there are active ingredients pulled based on the
#'     \code{chemicals} argument not present in the \code{chemical_class} data
#'     frame, these chemicals will be summed under the class "other".}
#'     \item{kg_chm_used}{A numeric value giving the amount of the active
#'     ingredient applied (kilograms).}
#'     \item{section}{A string nine characters long indicating the section
#'     of application. PLS sections are uniquely identified by a combination of
#'     base line meridian (S, M, or H), township (01-48), township direction
#'     (N or S), range (01-47), range direction (E or W) and section number
#'     (01-36). This column is not included if \code{sum_application = TRUE} and
#'     \code{unit = "township"}.}
#'     \item{township}{A string 7 characters long indicating the township
#'     of application. PLS townships are uniquely identified by a combination of
#'     base line meridian (S, M, or H), township (01-48), township direction
#'     (N or S), range (01-47), and range direction (E or W).}
#'     \item{county_name}{A character string giving the county name where
#'     application took place.}
#'     \item{county_code}{A string two characters long giving the PUR county
#'     code where application took place.}
#'     \item{date}{The date of application (yyyy-mm-dd).}
#'     \item{aerial_ground}{A character giving the application method.
#'     "A" = aerial, "G" = ground, and "O" = other. This column is not included if
#'     \code{sum_application = TRUE} and \code{include_ag = FALSE}.}
#'     \item{use_no}{A character string identifing unique application of an
#'     active ingredient across years. This value is a combination of the raw PUR
#'     \code{use_no} column and the year of application. Not included if
#'     \code{sum_appliction = TRUE}.}
#'     \item{outlier}{A logical value indicating whether the
#'     amount listed in \code{kg_chm_used} has been corrected large amounts
#'     entered in error. The algorithm for identifying and replacing outliers
#'     was developed based on methods used by Gunier et al. (2001). Please see
#'     the package vignette for more detail regarding these methods. Not included
#'     if \code{sum_application = TRUE}.}
#'  }
#'
#' @section Note:
#'   \itemize{
#'     \item The \code{chemical_list} data frame for a particular year lists
#'     active ingredients present in applied pesticides across the state of
#'     California. Therefore, PUR data for a particular county may not include
#'     records for active ingredients listed in the \code{chemical_list} dataset
#'     for the same year.
#'     \item To pull raw PUR data, see the \code{pull_raw_pur} function.
#'     For documentation of raw PUR data, see the Pesticide Use Report Data User
#'     Guide & Documentation document published by the California Department of
#'     Pesticide Regulation. This file is saved as "cd_doc.pdf" in
#'     any "pur[year].zip" file between 1990 and 2015 found here:
#'     \url{ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/}.
#' }
#'
#' @examples
#' \dontrun{
#' df <- clean_pur_data(download_progress = TRUE)
#' df2 <- clean_pur_data(years = 2000:2010,
#'                       counties = c("01", "nevada", "riverside"),
#'                       chemicals = "methylene")
#'
#' # Sum application by active ingredients
#' df3 <- clean_pur_data(years = 2000:2010,
#'                       counties = c("01", "nevada", "riverside"),
#'                       chemicals = "methylene",
#'                       unit = "township", sum_application = TRUE)
#'
#' # Or by chemical classes
#' chemical_class_df <- rbind(find_chemical_codes(2000, "methylene"),
#'                            find_chemical_codes(2000, "aldehyde")) %>%
#'    dplyr::rename(chemical_class = chemical)
#'
#' df4 <- clean_pur_data(years = 1995,
#'                       counties = "fresno",
#'                       chemicals = chemical_class$chemname,
#'                       sum_application = TRUE,
#'                       sum_by = "chemical_class",
#'                       unit = "township",
#'                       chemical_class = chemical_class_df)
#' }
#' @importFrom dplyr %>%
#' @export
clean_pur_data <- function(years = "all", counties = "all", chemicals = "all",
                           sum_application = FALSE, unit = "section",
                           sum_by = "active_ingredient", chemical_class = NULL,
                           include_ag = TRUE, verbose = TRUE,
                           download_progress = FALSE) {

  raw_df <- pull_raw_pur(years = years, counties = counties, verbose = verbose,
                         download_progress = download_progress)

  df <- raw_df %>%
    dplyr::mutate(township_pad = stringr::str_pad(raw_df$township, 2, "left", pad = "0"),
                  range = stringr::str_pad(raw_df$range, 2, "left", pad = "0"),
                  section = stringr::str_pad(raw_df$section, 2, "left", pad = "0"),
                  MTRS = as.character(paste0(base_ln_mer, township_pad, tship_dir,
                                             range, range_dir, section)),
                  township = as.character(paste0(base_ln_mer, township_pad, tship_dir)),
                  MTR = as.character(paste0(township, range, range_dir))) %>%
    dplyr::select(chem_code, lbs_chm_used, MTRS, MTR, county_cd, applic_dt,
                  aer_gnd_ind, use_no, acre_treated, unit_treated) %>%
    dplyr::mutate(unit_treated = as.factor(unit_treated)) %>%
    dplyr::filter(!MTRS %in% c(".0..0..0.", ".00.00.00", "NANANANANANA"),
                  unit_treated %in% c("A", "S")) %>%
    dplyr::mutate(acre_treated = as.numeric(acre_treated),
                  lbs_chm_used = as.numeric(lbs_chm_used),
                  acre_treated = ifelse(unit_treated == "S",
                                        acre_treated * 2.29568e-5,
                                        acre_treated),
                  unit_treated = "A", # going to remove this later - all acres
                  lbs_per_acre = lbs_chm_used/acre_treated,
                  chem_code = as.integer(chem_code))
  # correct for outliers (Gunier et al. (2001))
  # they used 1995 - I'm doing this for each year
  # if lbs per acre is larger than the calculated max rate of lbs per acre
  # (mean rate plus 2 standard deviations) then lbs_chm_used is replaced
  # with the calculated max rate multiplied by number of acres treated (acre_treated)
  ## might want to re-think this... PUR already has outlier control. (outlier90.txt)
  calc_max <- df %>%
    dplyr::group_by(chem_code) %>%
    dplyr::summarize(mean = mean(lbs_per_acre, na.rm = TRUE),
                     sd = sd(lbs_per_acre, na.rm = TRUE)) %>%
    dplyr::mutate(calc_max = mean + 2*sd)

  ## filter active ingredients, add chemname column

  if (!"all" %in% chemicals) {

    years_chemicals <- expand.grid(year = years, chemicals = chemicals) %>%
      dplyr::group_by(year) %>%
      tidyr::nest() %>%
      dplyr::mutate(chemicals = purrr::map(data, tibble_to_vector))

    chem_df <- purrr::map2_dfr(years_chemicals$year, years_chemicals$chemicals,
                               find_chemical_codes) %>% unique()

    df <- df %>%
      dplyr::filter(chem_code %in% chem_df$chem_code) %>%
      dplyr::left_join(chem_df, by = "chem_code") %>%
      dplyr::select(-chemical) %>%
      dplyr::mutate(applic_dt = lubridate::mdy(applic_dt))

    # chemicals
    if (nrow(df) == 0) {
      if (length(chemicals) == 1) {
        chem_message <- paste0(chemicals)
      } else if (length(chemicals) == 2) {
        chem_message <- paste0(chemicals[1], " or ", chemicals[2])
      } else if (length(chemicals) > 2) {
        chems_vec <- chemicals[1:length(chemicals)-1]
        chems_vec <- paste(chems_vec, collapse = ", ")
        chem_message <- paste0(chems_vec, ", or ", chemicals[length(chemicals)])
      }

      # years
      if (length(years) == 1) {
        year_message <- years
      } else if (length(years) == 2) {
        year_message <- paste0(years[1], " or ", years[2])
      } else if (length(years) > 1) {
        years_list <- split(years, cumsum(c(1, diff(years) != 1)))
        if (length(years_list) == 1) {
          year_message <- paste0(years[1], " through ", years[length(years)])
        } else {
          years_vec <- years[1:length(years)-1]
          years_vec <- paste(years_vec, collapse = ", ")
          year_message <- paste0(years_vec, ", or ", years[length(years)])
        }
      }

      # counties

      ##

      code_df <- purexposure::county_codes

      test <- suppressWarnings(as.numeric(counties))
      order <- data.frame(counties = counties, order = 1:length(counties))

      names <- counties[grep(TRUE, is.na(test))]
      if (length(names) != 0) {
        for (k in 1:length(names)) {

          s <- strsplit(names[k], " ")[[1]]
          county_name <- paste(toupper(substring(s, 1,1)), substring(s, 2),
                               sep = "", collapse = " ")
          order_df <- data.frame(counties = names[k], name_clean = county_name)
          if (k == 1) {
            county_name_out <- county_name
            order_df_out <- order_df
          } else {
            county_name_out <- c(county_name_out, county_name)
            order_df_out <- rbind(order_df_out, order_df)
          }
        }
      }

      codes <- counties[grep(FALSE, is.na(test))]
      if (length(codes) != 0) {
        for (l in 1:length(codes)) {

          county_name2 <- as.character(code_df %>%
                                         dplyr::filter(county_code == codes[l]) %>%
                                         dplyr::select(county_name))
          county_name2 <- tolower(county_name2)
          s2 <- strsplit(county_name2, " ")[[1]]
          county_name2 <- paste(toupper(substring(s2, 1,1)), substring(s2, 2),
                                sep = "", collapse = " ")
          order_df2 <- data.frame(counties = codes[l], name_clean = county_name2)
          if (l == 1) {
            county_name_out2 <- county_name2
            order_df_out2 <- order_df2
          } else {
            county_name_out2 <- c(county_name_out2, county_name2)
            order_df_out2 <- rbind(order_df_out2, order_df2)
          }
        }
      }

      if (length(names) != 0 & length(codes) != 0) {
        order_df <- rbind(order_df_out, order_df_out2)
      } else if (length(names) != 0 & length(codes) == 0) {
        order_df <- order_df_out
      } else if (length(names) == 0 & length(codes) != 0) {
        order_df <- order_df_out2
      }

      order_df_full <- suppressWarnings(dplyr::full_join(order_df, order,
                                                         by = "counties") %>%
                                          dplyr::arrange(order))

      names_clean <- as.character(order_df_full$name_clean)

      ##

      if (length(names_clean) == 1) {
        county_message <- paste0(names_clean, " county.")
      } else if (length(names_clean) == 2) {
        county_message <- paste0(names_clean[1], " or ", names_clean[2],
                                 " counties.")
      } else if (length(names_clean) > 2) {
        counties_vec <- names_clean[1:length(names_clean)-1]
        counties_vec <- paste(counties_vec, collapse = ", ")
        county_message <- paste0(counties_vec, ", or ",
                                 names_clean[length(names_clean)], " counties.")
      }

      stop(paste0("There weren't any pesticides containing ", chem_message,
                  " applied in ", year_message, " in ", county_message))
    }

  } else if ("all" %in% chemicals) {

    chem_df <- purexposure::chemical_list
    out_chem_list <- list()
    for (i in 1:length(years)) {
      chem_year <- chem_df[[as.character(years[i])]]
      chem_year <- chem_year %>% mutate(year = as.character(years[i]))
      out_chem_list[[i]] <- chem_year
    }
    out_chem_df <- dplyr::bind_rows(out_chem_list)

    df <- df %>% mutate(applic_dt = lubridate::mdy(applic_dt),
                        year = lubridate::year(applic_dt),
                        year = as.character(year)) %>%
      left_join(out_chem_df, by = c("chem_code", "year"))

  }

  df2 <- calc_max %>%
    dplyr::select(chem_code, calc_max) %>%
    dplyr::right_join(df, by = "chem_code") %>%
    dplyr::mutate(outlier = ifelse((!is.na(calc_max) &
                                      lbs_per_acre > calc_max), TRUE, FALSE),
                  lbs_chm_used = ifelse(lbs_per_acre > calc_max,
                                        calc_max*acre_treated, lbs_chm_used)) %>%
    dplyr::rename(county_code = county_cd)

  county <- purexposure::county_codes

  out <- county %>%
    dplyr::right_join(df2, by = "county_code") %>%
    dplyr::mutate(use_no = paste0(use_no, "_", lubridate::year(applic_dt)),
                  kg_chm_used = lbs_chm_used/2.20562) %>%
    dplyr::select(chem_code, chemname, kg_chm_used, MTRS, MTR, county_name,
                  county_code, applic_dt, aer_gnd_ind, use_no, outlier) %>%
    dplyr::rename(section = MTRS,
                  township = MTR,
                  date = applic_dt,
                  aerial_ground = aer_gnd_ind) %>%
    dplyr::arrange(date, county_name)

  # missing section and township IDs

  missing_sections <- grep("\\?", out$section, value = TRUE)
  missing_townships <- grep("\\?", out$township, value = TRUE)

  if (length(missing_sections) != 0) {
    out <- out %>% dplyr::mutate(section = ifelse(section %in% missing_sections,
                                                  NA, section))
  }

  if (length(missing_townships) != 0) {
    out <- out %>% dplyr::mutate(township = ifelse(township %in% missing_sections,
                                                  NA, township))
  }

  if (sum_application) {

    section_townships <- out %>% dplyr::select(section, township) %>%
      unique()

    if (sum_by == "active_ingredient") {
      if (unit == "section") {
        if (include_ag) {
          #1
          out <- out %>%
            dplyr::group_by(chem_code, chemname, section, county_name, county_code,
                            date, aerial_ground) %>%
            dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::left_join(section_townships, by = "section") %>%
            dplyr::arrange(date, county_name, county_code, chemname, chem_code,
                           township) %>%
            dplyr::select(chem_code, chemname, kg_chm_used, section, township,
                          county_name, county_code, date, aerial_ground)

        } else {
          #2
          out <- out %>%
            dplyr::group_by(chem_code, chemname, section, county_name, county_code,
                            date) %>%
            dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::left_join(section_townships, by = "section") %>%
            dplyr::arrange(date, county_name, county_code, chemname, chem_code,
                           township) %>%
            dplyr::select(chem_code, chemname, kg_chm_used, section, township,
                          county_name, county_code, date)

        }
      } else if (unit == "township") {
        if (include_ag) {
          #3
          out <- out %>%
            dplyr::group_by(chem_code, chemname, township, county_name, county_code,
                            date, aerial_ground) %>%
            dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(date, county_name, county_code, chemname, chem_code,
                           township) %>%
            dplyr::select(chem_code, chemname, kg_chm_used, township,
                          county_name, county_code, date, aerial_ground)
        } else {
          #4
          out <- out %>%
            dplyr::group_by(chem_code, chemname, township, county_name, county_code,
                            date) %>%
            dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(date, county_name, county_code, chemname, chem_code,
                           township) %>%
            dplyr::select(chem_code, chemname, kg_chm_used, township,
                          county_name, county_code, date)
        }
      }
    } else if (sum_by == "chemical_class") {

      ## error handling for chemical_class df
      if (!is.null(chemical_class) & !is.data.frame(chemical_class)) {
        stop("The chemical_class argument should be a data frame.")
      }
      if (!is.null(chemical_class) & is.data.frame(chemical_class) &
          !all(colnames(chemical_class) == c("chem_code", "chemname", "chemical_class"))) {
        stop(writeLines(paste0("The data frame entered in the chemical class ",
                               "argument should have three columns named chem_code, ",
                               "chemname, and chemical_class.")))
      }
      if (!is.null(chemical_class) & is.data.frame(chemical_class) &
          all(colnames(chemical_class) == c("chem_code", "chemname", "chemical_class")) &
          !is.integer(chemical_class$chem_code)) {
        stop("The chem_code column should have integer values.")
      }
      if (!is.null(chemical_class) & is.data.frame(chemical_class) &
          all(colnames(chemical_class) == c("chem_code", "chemname", "chemical_class")) &
          !is.character(chemical_class$chemname)) {
        stop("The chemname column should have character values.")
      }
      if (!is.null(chemical_class) & is.data.frame(chemical_class) &
          all(colnames(chemical_class) == c("chem_code", "chemname", "chemical_class")) &
          !is.character(chemical_class$chemical_class)) {
        stop("The chemical_class column should have character values.")
      }

      if (unit == "section") {
        if (include_ag) {
          #5
          out <- out %>%
            dplyr::left_join(chemical_class, by = c("chem_code", "chemname")) %>%
            dplyr::mutate(chemical_class = ifelse(is.na(chemical_class), "other",
                                                  chemical_class)) %>%
            dplyr::group_by(chemical_class, section, county_name, county_code,
                            date, aerial_ground) %>%
            dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::left_join(section_townships, by = "section") %>%
            dplyr::arrange(date, county_name, county_code, chemical_class,
                           township) %>%
            dplyr::select(chemical_class, kg_chm_used, section, township,
                          county_name, county_code, date, aerial_ground)

        } else {
          #6
          out <- out %>%
            dplyr::left_join(chemical_class, by = c("chem_code", "chemname")) %>%
            dplyr::mutate(chemical_class = ifelse(is.na(chemical_class), "other",
                                                  chemical_class)) %>%
            dplyr::group_by(chemical_class, section, county_name, county_code,
                            date) %>%
            dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::left_join(section_townships, by = "section") %>%
            dplyr::arrange(date, county_name, county_code, chemical_class,
                           township) %>%
            dplyr::select(chemical_class, kg_chm_used, section, township,
                          county_name, county_code, date)
        }
      } else if (unit == "township") {
        if (include_ag) {
          #7
          out <- out %>%
            dplyr::left_join(chemical_class, by = c("chem_code", "chemname")) %>%
            dplyr::mutate(chemical_class = ifelse(is.na(chemical_class), "other",
                                                  chemical_class)) %>%
            dplyr::group_by(chemical_class, township, county_name, county_code,
                            date, aerial_ground) %>%
            dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(date, county_name, county_code, chemical_class,
                           township) %>%
            dplyr::select(chemical_class, kg_chm_used, township,
                          county_name, county_code, date, aerial_ground)
        } else {
          #8
          out <- out %>%
            dplyr::left_join(chemical_class, by = c("chem_code", "chemname")) %>%
            dplyr::mutate(chemical_class = ifelse(is.na(chemical_class), "other",
                                                  chemical_class)) %>%
            dplyr::group_by(chemical_class, township, county_name, county_code,
                            date) %>%
            dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(date, county_name, county_code, chemical_class,
                           township) %>%
            dplyr::select(chemical_class, kg_chm_used, township,
                          county_name, county_code, date)
        }
      }
    }
  }

  return(out)

}
