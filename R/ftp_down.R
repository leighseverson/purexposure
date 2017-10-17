# for testing out functions when CDPR's FTP is down...
# pulls locally saved data for Fresno county, 1995.

pull_clean_pur2 <- function(years = 1995, counties = "fresno", chemicals = "all",
                           sum_application = FALSE, unit = "section",
                           sum = "all", chemical_class = NULL,
                           start_date = NULL, end_date = NULL,
                           aerial_ground = FALSE, verbose = TRUE,
                           download_progress = FALSE) {

  #  raw_df <- pull_raw_pur(years = years, counties = counties, verbose = verbose,
  #                        download_progress = download_progress)

  ### ftp is down

  raw_df <- readr::read_csv("~/Documents/pesticides_project/data-raw/PUR/1995/udc95_10.txt") %>%
    dplyr::mutate_all(as.character)

  ###

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
      } else if (length(chemicals) > 2 & length(chemicals) < 11) {
        chems_vec <- chemicals[1:length(chemicals)-1]
        chems_vec <- paste(chems_vec, collapse = ", ")
        chem_message <- paste0(chems_vec, ", or ", chemicals[length(chemicals)])
      } else if (length(chemicals) >= 11) {
        chem_message <- "these chemicals"
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
      names_clean <- find_counties(counties, return = "names")

      if (length(names_clean) == 1) {
        county_message <- paste0(names_clean, " county.")
      } else if (length(names_clean) == 2) {
        county_message <- paste0(names_clean[1], " or ", names_clean[2],
                                 " county.")
      } else if (length(names_clean) > 2) {
        counties_vec <- names_clean[1:length(names_clean)-1]
        counties_vec <- paste(counties_vec, collapse = ", ")
        county_message <- paste0(counties_vec, ", or ",
                                 names_clean[length(names_clean)], " county.")
      }

      stop(paste0("There weren't any pesticides containing ", chem_message,
                  " applied in ", year_message, " in ", county_message))
    }

  } else {

    chem_df <- purexposure::chemical_list
    out_chem_list <- list()
    for (i in 1:length(years)) {
      chem_year <- chem_df[[as.character(years[i])]]
      chem_year <- chem_year %>% dplyr::mutate(year = as.character(years[i]))
      out_chem_list[[i]] <- chem_year
    }
    out_chem_df <- dplyr::bind_rows(out_chem_list)

    df <- df %>% dplyr::mutate(applic_dt = lubridate::mdy(applic_dt),
                               year = lubridate::year(applic_dt),
                               year = as.character(year)) %>%
      dplyr::left_join(out_chem_df, by = c("chem_code", "year"))

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

    section_townships <- out %>%
      dplyr::select(section, township) %>%
      unique()

    if (sum == "all") {
      if (unit == "section") {
        if (aerial_ground) {
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
        if (aerial_ground) {
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
    } else if (sum == "chemical_class") {

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
        if (aerial_ground) {
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
        if (aerial_ground) {
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
  } else {
    if (!aerial_ground) {
      out <- out %>% dplyr::select(-aerial_ground)
    }
  }

  if (!is.null(start_date)) {
    out <- out %>% dplyr::filter(date >= lubridate::ymd(start_date))
  }
  if (!is.null(end_date)) {
    out <- out %>% dplyr::filter(date <= lubridate::ymd(end_date))
  }

  return(out)

}

# cleaned_pur_df should be from clean_pur_df2
calculate_exposure2 <- function(clean_pur_df, location, radius,
                               time_period = NULL, start_date = NULL,
                               end_date = NULL, chemicals = "all",
                               aerial_ground = FALSE) {

  # get numeric coordinate vector from location
  if (length(grep("-", location)) == 1) {
    latlon <- location
    latlon_vec <- as.numeric(as.vector(sapply(unlist(strsplit(latlon, ",")),
                                              stringr::str_trim)))
    address_x <- latlon_vec[1]
    address_y <- latlon_vec[2]
    latlon_out <- latlon_vec
  } else {
    address <- location
    suppressMessages(latlon_df <- ggmap::geocode(address, messaging = FALSE))
    address_x <- latlon_df$lon
    address_y <- latlon_df$lat
    latlon_out <- as.numeric(c(latlon_df$lon, latlon_df$lat))
  }

  # pull county shapefile
  county <- find_location_county(location, return = "name", latlon_out)
  check <- toupper(county) %in% clean_pur_df$county_name
  if (!check) {
    stop(paste0("\"", location,  "\"", " is located in ", county, " county. ",
                "\nThe clean_pur_df data frame doesn't include data for this ",
                "county."))
  }

  radius <- as.numeric(radius)

  # coordinates for buffer
  buffer <- geosphere::destPoint(p = c(address_x, address_y), b = 0:360,
                                 d = radius)
  colnames(buffer)[1] <- "long"
  buffer_df <- as.data.frame(buffer)
  range <- buffer_df %>% dplyr::summarise(min_long = min(long),
                                          min_lat = min(lat),
                                          max_long = max(long),
                                          max_lat = max(lat))

  # if ("section" %in% colnames(clean_pur_df)) {
  #   shp <- pull_spdf(county, "section")
  #   df <- spdf_to_df(shp)
  # } else {
  #   shp <- pull_spdf(county, "township")
  #   df <- spdf_to_df(shp)
  # }

  ### ftp is down (replaces ^)

  if ("section" %in% colnames(clean_pur_df)) {
    shp <- readRDS("~/Documents/pesticides_project/data/fresno_shp.rds")
    df <- readRDS("~/Documents/pesticides_project/data/fresno_coords.rds")
  } else {
    shp <- readRDS("~/Documents/pesticides_project/data/fresno_township_shp.rds")
    df <- readRDS("~/Documents/pesticides_project/data/fresno_township_coords.rds")
  }

  ###

  context_plot <- ggplot2::ggplot(data = df) +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group),
                          color = "grey", fill = NA) +
    ggplot2::geom_polygon(data = buffer_df, ggplot2::aes(x = long, y = lat),
                          color = "red", fill = NA) +
    ggplot2::theme_void()

  # find sections (and townships) w/in buffer
  which_pls <- df %>% dplyr::filter(long >= range$min_long &
                                      long <= range$max_long &
                                      lat >= range$min_lat &
                                      lat <= range$max_lat)

  if (nrow(which_pls) == 0) {

    if ("section" %in% colnames(clean_pur_df)) {

      borders <- df %>% group_by(MTRS) %>%
        dplyr::summarise(min_long = min(long), min_lat = min(lat),
                         max_long = max(long), max_lat = max(lat))

      corner <- purrr::map2_dfr(borders$max_long, borders$max_lat, euc_distance,
                                origin_long = range$max_long,
                                origin_lat = range$max_lat) %>%
        dplyr::filter(long > range$max_long & lat > range$max_lat) %>%
        dplyr::filter(dist == min(dist))

      closest_pls <- borders %>% dplyr::filter(max_long == corner$long,
                                               max_lat == corner$lat) %>%
        dplyr::select(MTRS) %>%
        tibble_to_vector()

      which_pls <- df %>% dplyr::filter(MTRS == closest_pls)

    } else {

      borders <- df %>% group_by(MTR) %>%
        dplyr::summarise(min_long = min(long), min_lat = min(lat),
                         max_long = max(long), max_lat = max(lat))

      corner <- purrr::map2_dfr(borders$max_long, borders$max_lat, euc_distance,
                                origin_long = range$max_long,
                                origin_lat = range$max_lat) %>%
        dplyr::filter(long > range$max_long & lat > range$max_lat) %>%
        dplyr::filter(dist == min(dist))

      closest_pls <- borders %>% dplyr::filter(max_long == corner$long,
                                               max_lat == corner$lat) %>%
        dplyr::select(MTR) %>%
        tibble_to_vector()

      which_pls <- df %>% dplyr::filter(MTR == closest_pls)

    }
  }

  # data frame with start and end dates
  if (!is.null(time_period)) {
    # multiple time periods
    if (is.null(start_date)) {
      start_date <- min(clean_pur_df$date)
    } else {
      start_date <- lubridate::ymd(start_date)
    }
    if (is.null(end_date)) {
      end_date <- max(clean_pur_df$date)
    } else {
      end_date <- lubridate::ymd(end_date)
    }

    date_seq <- seq(start_date, end_date, by = time_period)
    end_dates <- dplyr::lead(date_seq, 1) - lubridate::days(1)

    time_df <- data.frame(start_date = date_seq,
                          end_date = end_dates)
    time_df$end_date[nrow(time_df)] <- end_date

  } else {

    # one time period
    if (is.null(start_date)) {
      start_date <- min(clean_pur_df$date)
    } else {
      start_date <- lubridate::ymd(start_date)
    }
    if (is.null(end_date)) {
      end_date <- max(clean_pur_df$date)
    } else {
      end_date <- lubridate::ymd(end_date)
    }

    time_df <- data.frame(start_date = start_date, end_date = end_date)

  }

  # check_dates
  check_min_date <- min(time_df$start_date) == min(clean_pur_df$date)
  check_max_date <- max(time_df$end_date) == max(clean_pur_df$date)

  if (!check_min_date | !check_max_date) {
    stop("Your pur_clean_df data frame doesn't contain data for the entire date\n ",
         "range speficied by start_date and end_date. You may need to pull data\n ",
         "for additional years using the pull_clean_pur() function." )
  }

  if ("section" %in% colnames(clean_pur_df)) {
    out_list <- pur_filt_df(MTRS, "MTRS")
  } else {
    out_list <- pur_filt_df(MTR, "MTR")
  }

  pur_filt <- out_list$pur_filt
  comb_df_filt <- out_list$comb_df_filt
  pls_percents <- out_list$pls_intersections
  pls_int <- out_list$pls_int

  out <- purrr::map2(time_df$start_date, time_df$end_date,
                     calculate_exposure_bydate)
  for (i in 1:length(out)) {
    exp_row <- out[[i]]$row_out[[1]]
    meta_data <- out[[i]]$meta_data[[1]]
    if (i == 1) {
      row_out <- exp_row
      meta_out <- meta_data
    } else {
      row_out <- rbind(row_out, exp_row)
      meta_out <- rbind(meta_out, meta_data)
    }
  }

  out <- list(exposure = row_out,
              meta_data = meta_out,
              buffer_plot_df = comb_df_filt,
              county_plot = context_plot)

  return(out)

}

