#' @importFrom magrittr %>%
help_pull_pur <- function(year, counties = "all", download_progress = TRUE) {

  current_dir <- getwd()

  url <- paste0("ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives/pur",
                year, ".zip")
  file <- paste0("pur", year, ".zip")

  dir <- tempdir()
  setwd(dir)

  if (download_progress) {
    quiet <- FALSE
  } else {
    quiet <- TRUE
  }

  download.file(url, destfile = file, mode = "wb", quiet = quiet)
  unzip(file, exdir = dir)

  sm_year <- substr(year, 3, 4)

  if (!"all" %in% counties) {

    codes <- find_counties(counties)

    counties_in_year <- purrr::map_dfr(codes, help_read_in_counties, type = "codes",
                                       year = year) %>%
      dplyr::arrange(applic_dt, county_cd)

  } else {

    files <- grep(paste0("udc", sm_year, "_"), list.files(), value = TRUE)

    counties_in_year <- purrr::map_dfr(files, help_read_in_counties, type = "files",
                                       year = year) %>%
      dplyr::arrange(applic_dt, county_cd)

  }

  setwd(current_dir)

  return(counties_in_year)

}

help_read_in_counties <- function(code_or_file, type, year) {

  sm_year <- substr(year, 3, 4)

  if (type == "codes") {

    raw_data <- suppressWarnings(suppressMessages(
      readr::read_csv(paste0("udc", sm_year, "_", code_or_file, ".txt"),
                      progress = FALSE)))
    raw_data <- dplyr::mutate_all(raw_data, as.character)

  } else if (type == "files") {

    raw_data <- suppressWarnings(suppressMessages(
      readr::read_csv(code_or_file, progress = FALSE)))
    raw_data <- dplyr::mutate_all(raw_data, as.character)

  }

  # neither of these columns are documented in PUR guide, and they
  # are inconsistently included in PUR datasets.
  if ("error_flag" %in% colnames(raw_data)) {

    raw_data <- raw_data %>%
      dplyr::select(-error_flag)

  }

  if ("comtrs" %in% colnames(raw_data)) {

    raw_data <- raw_data %>%
      dplyr::select(-comtrs)

  }

  date_min <- lubridate::ymd(paste0(year, "-01-01"))
  date_max <- lubridate::ymd(paste0(year, "-12-31"))

  # sometimes 12/31 from the previous year is included...
  raw_data <- raw_data %>%
    dplyr::mutate(applic_dt = lubridate::mdy(applic_dt)) %>%
    dplyr::filter(applic_dt >= date_min &
                    applic_dt <= date_max) %>%
    dplyr::mutate(applic_dt = as.character(applic_dt))

  return(raw_data)

}

#' @importFrom magrittr %>%
help_find_code <- function(county, return = "codes") {

  code_df <- purexposure::county_codes

  test <- suppressWarnings(as.numeric(county))

  if (is.na(test)) {

    county_upper <- toupper(county)
    county_nm <- grep(county_upper, code_df$county_name, value = TRUE)

    if (length(county_nm) != 1) {
      error <- "yes"
    } else {
      error <- NULL

      code <- as.character(code_df %>%
                             dplyr::filter(county_name == county_nm) %>%
                             dplyr::select(county_code))

      name <- strsplit(tolower(county_nm), " ")[[1]]
      name <- paste(toupper(substring(name, 1,1)), substring(name, 2),
                    sep = "", collapse = " ")
    }

  } else {

    county_cd <- county
    code <- grep(county_cd, code_df$county_code, value = TRUE)

    if (length(code) != 1) {
      error <- "yes"
    } else {
      error <- NULL

      county_nm <- as.character(code_df %>%
                                  dplyr::filter(county_code == code) %>%
                                  dplyr::select(county_name))

      name <- strsplit(tolower(county_nm), " ")[[1]]
      name <- paste(toupper(substring(name, 1,1)), substring(name, 2),
                    sep = "", collapse = " ")
    }
  }

  if (is.null(error)) {
    if (return == "codes") {
      return(code)
    } else if (return == "names") {
      return(name)
    }
  } else {
    return(NULL)
  }

}

#' @importFrom magrittr %>%
#' @importFrom rlang !!!
help_sum_application <- function(df, sum, unit, aerial_ground,
                                 section_townships, chemical_class = NULL,
                                 ...) {

  group_by_vars <- rlang::quos(...)

  if (sum == "chemical_class") {
    df <- df %>%
      dplyr::left_join(chemical_class, by = c("chem_code", "chemname")) %>%
      dplyr::mutate(chemical_class = ifelse(is.na(chemical_class), "other",
                                            chemical_class))
  }

  df <- df %>%
    dplyr::group_by(!!!group_by_vars) %>%
    dplyr::summarise(kg_chm_used = sum(kg_chm_used, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if (unit == "section") {
    df <- df %>% dplyr::left_join(section_townships, by = "section")
  }

  all_cols <- c("chem_code", "chemname", "chemical_class", "kg_chm_used",
                "section", "township", "county_name", "county_code",
                "date", "aerial_ground", "use_no", "outlier")

  missing <- all_cols[!all_cols %in% colnames(df)]

  for (i in 1:length(missing)) {
    new_col <- rlang::quo_name(missing[i])
    df <- df %>% dplyr::mutate(!!new_col := NA)
  }

  df <- df %>%
    dplyr::arrange(date, county_name, county_code, chemical_class, chemname,
                   chem_code, section, township) %>%
    dplyr::select(chem_code, chemname, chemical_class, kg_chm_used,
                  section, township, county_name, county_code,
                  date, aerial_ground, use_no, outlier)

  cols_to_remove <- purrr::map_dfr(colnames(df), help_remove_cols, df = df) %>%
    dplyr::filter(all_missing == T) %>%
    dplyr::select(col) %>%
    tibble_to_vector()

  cols_to_keep <- colnames(df)[!colnames(df) %in% cols_to_remove]
  cols_names <- unlist(purrr::map(cols_to_keep, rlang::quo_name))
  df <- df %>% dplyr::select(!!cols_names)

  return(df)

}

#' @importFrom magrittr %>%
#' @importFrom rlang !!
help_remove_cols <- function(col_quote, df) {

  col_name <- rlang::quo_name(col_quote)
  vals <- df %>% dplyr::select(!!col_name) %>% tibble_to_vector()
  logical_val <- all(is.na(vals))
  out_df <- data.frame(col = col_name, all_missing = logical_val)

  return(out_df)

}

#' @importFrom magrittr %>%
#' @importFrom rlang !!
help_filter_pls <- function(pls, pls_quote, which_pls, shp, buffer, df,
                        clean_pur_df) {

  pls_var <- rlang::enquo(pls)
  pls_name <- rlang::quo_name(pls_quote)

  pls_int <- which_pls %>%
    tibble::as_tibble() %>%
    dplyr::select(!!pls_name) %>%
    unique() %>%
    tibble_to_vector()

  # filter shp file to include only sections intersecting w/ buffer-
  df_filtered <- df %>% dplyr::filter(rlang::UQ(pls_var) %in% pls_int)

  # combine buffer to existing sections shp file
  buffer_shp <- shp[1, ]
  buffer_shp@polygons[[1]]@Polygons[[1]]@coords <- buffer
  buffer_shp <- sp::spChFIDs(buffer_shp, paste("buffer", 1:nrow(buffer_shp), sep = ""))

  if (pls_quote == "MTRS") {
    buffer_shp@data$MTRS <- "buffer"
  } else {
    buffer_shp@data$MTR <- "buffer"
  }

  comb_shp <- maptools::spRbind(shp, buffer_shp)
  suppressMessages(comb_df <- broom::tidy(comb_shp))

  if (pls_quote == "MTRS") {
    comb_shp_filt <- subset(comb_shp, MTRS %in% c(pls_int, "buffer"))
  } else {
    comb_shp_filt <- subset(comb_shp, MTR %in% c(pls_int, "buffer"))
  }

  buffer_poly <- length(comb_shp_filt@polygons)
  section_polys <- 1:(length(comb_shp_filt@polygons) - 1)

  for (i in 1:length(section_polys)) {

    section_area <- comb_shp_filt@polygons[[i]]@Polygons[[1]]@area
    intersection <- rgeos::gIntersection(sp::SpatialPolygons(comb_shp_filt@polygons[i]),
                                         sp::SpatialPolygons(comb_shp_filt@polygons[buffer_poly]))

    if (is.null(intersection)) {
      intersected_area <- 0
    } else {
      intersected_area <- rgeos::gArea(intersection)
    }

    if (pls_quote == "MTRS") {
      percent_df <- data.frame(MTRS = comb_shp_filt@data$MTRS[i],
                               percent = intersected_area/section_area)
    } else {
      percent_df <- data.frame(MTR = comb_shp_filt@data$MTR[i],
                               percent = intersected_area/section_area)
    }

    if (i == 1) {
      out <- percent_df
    } else {
      out <- rbind(out, percent_df)
    }
  }

  out <- dplyr::filter(out, percent != 0)
  if (pls_quote == "MTRS") {
    comb_shp_filt <- subset(comb_shp_filt, MTRS %in% c(as.character(out$MTRS),
                                                       "buffer"))
  } else {
    comb_shp_filt <- subset(comb_shp_filt, MTR %in% c(as.character(out$MTR),
                                                      "buffer"))
  }

  suppressMessages(comb_df_filt <- broom::tidy(comb_shp_filt))

  comb_df_filt <- dplyr::mutate(comb_df_filt, group = as.character(group))

  comb_df_filt <- df_filtered %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::right_join(comb_df_filt, by = c("long", "lat", "hole", "order",
                                           "piece", "id", "group"))

  pur_filt <- dplyr::filter(clean_pur_df, !is.na(kg_chm_used))
  out <- list(pur_filt = pur_filt,
              comb_df_filt = comb_df_filt,
              pls_intersections = out,
              pls_int = pls_int)

  return(out)

}

#' @importFrom magrittr %>%
#' @importFrom rlang !!!
help_sum_ai <- function(pur_filt, start_date, end_date, ...) {

  group_by <- rlang::quos(...)

  pur_summed <- pur_filt %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::group_by(!!!group_by) %>%
    dplyr::summarise(kg = sum(kg_chm_used)) %>%
    dplyr::ungroup()

  return(pur_summed)

}

#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom rlang :=
help_write_md <- function(clean_pur_df, pls_percents, pur_out, location,
                   start_date, end_date, radius, buffer_area,
                   mtrs_mtr, section_township) {

  mutate_expr <- rlang::enquo(mtrs_mtr)
  rename_expr <- rlang::enquo(section_township)

  if ("chemical_class" %in% colnames(clean_pur_df)){

    classes <- unique(clean_pur_df$chemical_class)
    n_classes <- length(classes)
    pls <- as.character(pls_percents[,1])
    n_pls <- length(pls)

    class_pls <- data.frame(pls = as.character(rep(pls, times = n_classes)),
                            chemicals = rep(classes, each = n_pls)) %>%
      dplyr::mutate(pls = as.character(pls))

    pur_out2 <- pur_out %>%
      dplyr::rename(pls := !!rename_expr,
                    chemicals = chemical_class) %>%
      dplyr::mutate(chemicals = as.character(chemicals))

    exp_0 <- pls_percents %>%
      dplyr::rename(pls := !!mutate_expr) %>%
      dplyr::mutate(pls = as.character(pls)) %>%
      dplyr::full_join(class_pls, by = "pls") %>%
      dplyr::arrange(chemicals) %>%
      dplyr::mutate(chemicals = as.character(chemicals)) %>%
      dplyr::left_join(pur_out2, by = c("pls", "chemicals")) %>% # brings in kg, maybe aerial_ground
      dplyr::mutate(location = location,
                    kg = ifelse(is.na(kg), 0, kg),
                    none_recorded = ifelse(kg == 0, TRUE, FALSE),
                    kg_intersection = percent * kg,
                    start_date = start_date,
                    end_date = end_date,
                    radius = radius,
                    area = buffer_area)

  } else {

    class_pls <- data.frame(pls = as.character(pls_percents[,1]),
                            chemicals = "all") %>%
      dplyr::mutate(pls = as.character(pls))

    pur_out2 <- pur_out %>%
      dplyr::rename(pls := !!rename_expr)

    exp_0 <- pls_percents %>%
      dplyr::rename(pls := !!mutate_expr) %>%
      dplyr::mutate(pls = as.character(pls)) %>%
      dplyr::full_join(class_pls, by = "pls") %>%
      dplyr::mutate(chemicals = as.character(chemicals)) %>%
      dplyr::left_join(pur_out2, by = "pls") %>% # brings in kg, maybe aerial_ground
      dplyr::mutate(location = location,
                    kg = ifelse(is.na(kg), 0, kg),
                    none_recorded = ifelse(kg == 0, TRUE, FALSE),
                    kg_intersection = percent * kg,
                    start_date = start_date,
                    end_date = end_date,
                    radius = radius,
                    area = buffer_area)

  }

  if ("aerial_ground" %in% colnames(exp_0)){
    exp <- exp_0 %>% dplyr::mutate(aerial_ground = as.character(aerial_ground))
  } else {
    exp <- exp_0 %>% dplyr::mutate(aerial_ground = NA)
  }

  exp <- exp %>%
    dplyr::select(pls, chemicals, percent, kg, kg_intersection, start_date,
                  end_date, aerial_ground, none_recorded, location, radius, area)

  return(exp)

}

#' @importFrom magrittr %>%
#' @importFrom rlang !!!
help_calc_exp <- function(exp, buffer_area, ...) {

  group_by_vars <- rlang::quos(...)

  exp_out <- exp %>%
    dplyr::group_by(!!!group_by_vars) %>%
    dplyr::summarise(exposure = sum(kg_intersection, na.rm = TRUE) / buffer_area)

  return(exp_out)

}

#' @importFrom magrittr %>%
#' @importFrom rlang !!!
help_return_exposure <- function(start_date, end_date, location, radius,
                                 exp, buffer_area, ...) {

  vars <- rlang::quos(...)

  row_out_0 <- help_calc_exp(exp, buffer_area, !!!vars)

  if ("aerial_ground" %in% colnames(row_out_0)) {

    row_out <- row_out_0 %>%
      dplyr::mutate(start_date = start_date,
                    end_date = end_date,
                    location = location,
                    radius = radius) %>%
      dplyr::select(exposure, chemicals, start_date, end_date, aerial_ground,
                    location, radius)

  } else {

    row_out <- row_out_0 %>%
      dplyr::mutate(start_date = start_date,
                    end_date = end_date,
                    aerial_ground = NA,
                    location = location,
                    radius = radius) %>%
      dplyr::select(exposure, chemicals, start_date, end_date, aerial_ground,
                    location, radius)

  }

  return(row_out)

}

#' @importFrom magrittr %>%
help_calculate_exposure <- function(start_date, end_date, aerial_ground,
                                    chemicals, clean_pur_df, location,
                                    pls_percents, pur_filt, radius) {

  if (chemicals == "all") {
    if ("section" %in% colnames(pur_filt)) {
      if (aerial_ground) {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                               section, aerial_ground)
      } else {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                               section)
      }
    } else {
      if (aerial_ground) {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                               township, aerial_ground)
      } else {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date, township)
      }
    }
  } else {
    if ("section" %in% colnames(pur_filt)) {
      if (aerial_ground) {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                               section, chemical_class, aerial_ground)
      } else {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                               section, chemical_class)
      }
    } else {
      if (aerial_ground) {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                               township, chemical_class, aerial_ground)
      } else {
        pur_out <- help_sum_ai(pur_filt, start_date, end_date,
                               township, chemical_class)
      }
    }
  }

  buffer_area <- pi * (radius^2)

  if ("section" %in% colnames(pur_filt)) {
    exp <- help_write_md(clean_pur_df, pls_percents, pur_out, location, start_date,
                         end_date, radius, buffer_area,
                         MTRS, section)
  } else {
    exp <- help_write_md(clean_pur_df, pls_percents, pur_out, location, start_date,
                         end_date, radius, buffer_area,
                         MTR, township)
  }

  if (aerial_ground) {
    row_out <- help_return_exposure(start_date, end_date, location, radius, exp,
                                    buffer_area,
                                    chemicals, aerial_ground)
  } else {
    row_out <- help_return_exposure(start_date, end_date, location, radius, exp,
                                    buffer_area, chemicals)
  }

  nested_df <- list(row_out = list(row_out), meta_data = list(exp))
  attr(nested_df, "row.names") <- as.integer(1)
  class(nested_df) <- c("tbl_df", "data.frame")

  return(nested_df)

}

#' @importFrom magrittr %>%
help_map_exp <- function(start_date, end_date, chemicals, aerial_ground,
                     none_recorded, data_pls,
                     gradient, location_longitude,
                     location_latitude, buffer_df, buffer2, buffer,
                     buffer_or_county, alpha, clean_pur, pls_labels,
                     pls_labels_size, percentile, color_by) {

  pls_df <- buffer_df %>%
    dplyr::right_join(data_pls, by = "pls")

  section_or_township <- unique(pls_df$section_or_township)

  if (section_or_township == "MTRS") {
    s_t <- "section"
  } else if (section_or_township == "MTR") {
    s_t <- "township"
  }

  legend_label <- paste0("Applied Pesticides\n(kg/", s_t, ")")

  if (!none_recorded) {

    full <- dplyr::filter(data_pls, percent > 0.999)
    full_pls_df <- buffer_df %>%
      dplyr::right_join(full, by = "pls") %>%
      dplyr::select(long, lat, group, kg_intersection) %>%
      dplyr::rename(kg = kg_intersection) %>%
      unique()

    partial <- dplyr::filter(data_pls, percent <= 0.999)
    partial_pls_df <- buffer_df %>%
      dplyr::right_join(partial, by = "pls")

    pls_partials <- unique(partial_pls_df$pls)

    if (length(pls_partials) != 0) {
      for (i in 1:length(pls_partials)) {

        df2 <- dplyr::filter(partial_pls_df, pls == pls_partials[i])

        pls <- dplyr::select(df2, long, lat)
        pls <- pls[grDevices::chull(pls), ]
        pls <- methods::as(pls, "gpc.poly")

        suppressWarnings(
          intersection <- raster::intersect(pls, buffer)
        )
        # Warning message:
        #   In `[<-`(`*tmp*`, i, value = gpc) :
        #   implicit list embedding of S4 objects is deprecated

        int_df <- as.data.frame(methods::as(intersection, "matrix")) %>%
          dplyr::rename(long = x,
                        lat = y) %>%
          dplyr::mutate(group = paste0("int", i),
                        kg = unique(df2$kg_intersection))

        if (i == 1) {
          out_int <- int_df
        } else {
          out_int <- rbind(out_int, int_df)
        }
      }

      if (nrow(full_pls_df) != 0) {

        section_data <- rbind(out_int, full_pls_df)

      } else {

        section_data <- out_int

      }

    } else if (nrow(full_pls_df) != 0) {

      section_data <- full_pls_df

    }

    plot <- section_data %>%
      ggplot2::ggplot() +
      ggplot2::theme_void()

    if (color_by == "amount") {

      if (buffer_or_county == "buffer") {

        plot <- plot +
          ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = kg),
                       color = "black") +
          scale_fill_gradientn2(colours = gradient, alpha = alpha, name = legend_label,
                                na.value = "#FFFFFF")

      } else {

        ##

        if (!is.na(aerial_ground)) {
          clean_pur <- clean_pur %>%
            dplyr::filter(aerial_ground == aerial_ground)
        }

        if (chemicals != "all") {
          if ("chemical_class" %in% colnames(clean_pur)) {
            clean_pur <- clean_pur %>%
              dplyr::filter(chemical_class == chemicals)
          }
        }

        if (s_t == "section") {

          clean_pur2 <- clean_pur %>%
            dplyr::filter(date >= lubridate::ymd(start_date) &
                            date <= lubridate::ymd(end_date)) %>%
            dplyr::group_by(section) %>%
            dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE))

        } else if (s_t == "township") {

          clean_pur2 <- clean_pur %>%
            dplyr::filter(date >= lubridate::ymd(start_date) &
                            date <= lubridate::ymd(end_date)) %>%
            dplyr::group_by(township) %>%
            dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE))

        }

        clean_pur3 <- clean_pur2 %>%
          dplyr::mutate(source = "county")

        limits <- c(min(clean_pur3$kg, na.rm = TRUE),
                    max(clean_pur3$kg, na.rm = TRUE))

        ##
        plot <- plot +
          ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group,
                                             fill = kg),
                                color = "black") +
          scale_fill_gradientn2(colours = gradient, alpha = alpha, name = legend_label,
                                limits = limits, na.value = "#FFFFFF")
      }



    } else if (color_by == "percentile") {

      cutpoint_list <- help_categorize(section_data,
                                       buffer_or_county = buffer_or_county,
                                       start_date, end_date, aerial_ground,
                                       chemicals, clean_pur, s_t, percentile)

      cutoff_values <- cutpoint_list$cutoff_values

      section_data2 <- cutpoint_list$df

      categories <- cutpoint_list$categories

      if ("None recorded" %in% categories) {
        n_cols <-  as.integer(length(gradient)/(length(categories)-1))
        end_i <- length(categories)-1
      } else {
        n_cols <- as.integer(length(gradient)/length(categories))
        end_i <- length(categories)
      }
      for (i in 1:end_i) {
        col_vec <- gradient[n_cols*i]
        if (i == 1) {
          out <- col_vec
        } else {
          out <- c(out, col_vec)
        }
      }

      if ("None recorded" %in% categories) {
        out <- c(out, "#FFFFFF")
      }

      names(out) <- categories

      plot <- plot +
        ggplot2::geom_polygon(data = section_data2,
                              ggplot2::aes(x = long, y = lat, group = group,
                                           fill = category), color = "black") +
        ggplot2::scale_fill_manual(values = out, name = legend_label)

    }

    plot <- plot + ggplot2::geom_polygon(data = buffer_df,
                                         ggplot2::aes(x = long, y = lat, group = group),
                                         color ="black", fill = NA) +
      ggplot2::geom_point(x = location_longitude, y = location_latitude, size = 2)

    if (pls_labels) {

      df_all <- dplyr::select(pls_df, pls, DDLONG, DDLAT) %>% unique()

      plot <- plot +
        ggplot2::geom_text(data = df_all, ggplot2::aes(x = DDLONG, y = DDLAT,
                                                       label = pls),
                           size = pls_labels_size, fontface = "bold")

    }

    data_pls <- data_pls %>%
      dplyr::mutate(start_date = zoo::as.Date(start_date),
                    end_date = zoo::as.Date(end_date),
                    chemicals = chemicals, aerial_ground = aerial_ground) %>%
      dplyr::select(pls, percent, kg, kg_intersection, start_date, end_date,
                    chemicals, aerial_ground, none_recorded, location, radius,
                    area)

  } else {

    missing_buffer_df <- buffer_df %>% dplyr::mutate(perc_fill = "None recorded",
                                                     scale_fill = "0")

    if (color_by == "percentile") {

      missing_plot <- ggplot2::ggplot(missing_buffer_df) +
        ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group,
                                           fill = perc_fill), color = "black") +
        ggplot2::geom_point(x = location_longitude, y = location_latitude, color = "black",
                   size = 2) +
        ggplot2::theme_void() +
        ggplot2::scale_fill_manual(name = legend_label, values = c("None recorded" = NA))

    } else if (color_by == "amount") {

      missing_plot <- ggplot2::ggplot(missing_buffer_df) +
        ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group,
                                           fill = scale_fill), color = "black") +
        ggplot2::geom_point(x = location_longitude, y = location_latitude, color = "black",
                   size = 2) +
        ggplot2::theme_void() +
        ggplot2::scale_fill_manual(name = legend_label, values = c("0" = NA))

    }

    plot <- missing_plot
    start_date <- zoo::as.Date(start_date)
    end_date <- zoo::as.Date(end_date)
    data_pls <- data.frame(pls = "ALL", percent = NA,
                           kg = 0, kg_intersection = NA,
                           start_date = start_date,
                           end_date = end_date,
                           chemicals = NA,
                           aerial_ground = NA,
                           none_recorded = TRUE,
                           location = unique(data_pls$location),
                           radius = unique(data_pls$radius),
                           area = unique(data_pls$area))

    cutoff_values <- data.frame(percentile = percentile, kg = NA)

  }

  if (color_by == "amount") {
    return(list(plot = plot, data = data_pls))
  } else if (color_by == "percentile") {
    return(list(plot = plot, data = data_pls, cutoff_values = cutoff_values))
  }



}

#' @importFrom magrittr %>%
help_categorize <- function(section_data, buffer_or_county,
                           start_date = NULL, end_date = NULL,
                           aerial_ground = NULL, chemicals = NULL,
                           clean_pur = NULL, s_t = NULL, percentile) {

  if (buffer_or_county == "buffer") {

    perc <- as.data.frame(t(quantile(unique(section_data$kg),
                                     probs = percentile, na.rm = TRUE)))
    vec <- 0
    for (i in 1:length(percentile)) {
      vec <- c(vec, perc[, i])
    }
    vec <- c(vec, max(unique(section_data$kg),
                      na.rm = TRUE))
    perc_numbers <- as.character(percentile * 100)
    first <- paste0("<=", perc_numbers[1], "th percentile")
    last <- paste0(">=", perc_numbers[length(perc_numbers)], "th")

    for (i in 1:(length(perc_numbers) - 1)) {
      label <- paste0(">=", perc_numbers[i], "th to <", perc_numbers[i+1], "th")
      if (i == 1) {
        middle <- label
      } else {
        middle <- c(middle, label)
      }
    }

    labels <- c(first, middle, last)

    df_out <- section_data %>%
      dplyr::mutate(category = as.character(cut(section_data$kg, vec, labels = labels)),
                    category = ifelse(is.na(category), "None recorded", category))

    if ("None recorded" %in% unique(df_out$category)) {
      df_out$category <- factor(df_out$category, levels = c(labels, "None recorded"))
    } else {
      df_out$category <- factor(df_out$category, levels = labels)
    }

  } else if (buffer_or_county == "county") {

    if (!is.na(aerial_ground)) {
      clean_pur <- clean_pur %>%
        dplyr::filter(aerial_ground == aerial_ground)
    }

    if (chemicals != "all") {
      if ("chemical_class" %in% colnames(clean_pur)) {
        clean_pur <- clean_pur %>%
          dplyr::filter(chemical_class == chemicals)
      }
    }

    if (s_t == "section") {

      clean_pur2 <- clean_pur %>%
        dplyr::filter(date >= lubridate::ymd(start_date) &
                        date <= lubridate::ymd(end_date)) %>%
        dplyr::group_by(section) %>%
        dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE))

    } else if (s_t == "township") {

      clean_pur2 <- clean_pur %>%
        dplyr::filter(date >= lubridate::ymd(start_date) &
                        date <= lubridate::ymd(end_date)) %>%
        dplyr::group_by(township) %>%
        dplyr::summarise(kg = sum(kg_chm_used, na.rm = TRUE))

    }

    clean_pur3 <- clean_pur2 %>%
      dplyr::mutate(source = "county")

    section_data <- section_data %>%
      dplyr::mutate(source = "buffer")

    to_join <- section_data %>%
      dplyr::select(group, kg, source)

    all_pls <- clean_pur3 %>%
      dplyr::full_join(to_join, by = c("kg", "source"))

    perc <- as.data.frame(t(quantile(unique(all_pls$kg),
                                     probs = percentile, na.rm = TRUE)))
    vec <- 0
    for (i in 1:length(percentile)) {
      vec <- c(vec, perc[, i])
    }
    vec <- c(vec, max(unique(all_pls$kg),
                      na.rm = TRUE))
    perc_numbers <- as.character(percentile * 100)
    first <- paste0("<=", perc_numbers[1], "th percentile")
    last <- paste0(">=", perc_numbers[length(perc_numbers)], "th")

    for (i in 1:(length(perc_numbers) - 1)) {
      label <- paste0(">=", perc_numbers[i], "th to <", perc_numbers[i+1], "th")
      if (i == 1) {
        middle <- label
      } else {
        middle <- c(middle, label)
      }
    }

    labels <- c(first, middle, last)

    all_pls_out <- all_pls %>%
      dplyr::mutate(category = as.character(cut(all_pls$kg, vec, labels = labels)),
                    category = ifelse(is.na(category), "None recorded", category))

    df_out2 <- section_data %>%
      dplyr::mutate(category = as.character(cut(section_data$kg, vec, labels = labels)),
                    category = ifelse(is.na(category), "None recorded", category))

    buffer_pls <- df_out2 %>%
      dplyr::filter(source == "buffer") %>%
      dplyr::select(kg, source, group, category) %>%
      dplyr::full_join(section_data, c("kg", "source", "group")) %>%
      dplyr::select(long, lat, group, kg, category)

    if ("None recorded" %in% unique(buffer_pls$category)) {
      buffer_pls$category <- factor(buffer_pls$category, levels = c(labels, "None recorded"))
    } else {
      buffer_pls$category <- factor(buffer_pls$category, levels = labels)
    }

    df_out <- buffer_pls

  }

  if ("None recorded" %in% unique(df_out$category)) {
    labels <- c(labels, "None recorded")
  }

  percents <- sub("%", "", colnames(perc)) %>% as.numeric() / 100
  cutoff <- perc %>% tidyr::gather("percentile", "kg") %>%
    dplyr::mutate(percentile = percents)

  out <- list(df = df_out, categories = labels, cutoff_values = cutoff)

  return(out)

}

#' @importFrom magrittr %>%
help_find_chemical <- function(chemical, df) {

  quotemeta <- function(string) {
    stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
  }

  chem_up <- toupper(chemical)
  chem_up <- substr(chem_up, 1, 50)
  if (chem_up == "ALL") {
    df2 <- df
  } else {
    df2 <- df[grep(quotemeta(chem_up), df$chemname), ]
    df2 <- df2 %>% dplyr::mutate(chemical = chemical)
  }

  return(df2)

}

#' @importFrom magrittr %>%
help_find_product <- function(product, df) {

  quotemeta <- function(string) {
    stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
  }

  prod_up <- toupper(product)
  prod_up <- substr(prod_up, 1, 50)
  if (prod_up == "ALL") {
    df2 <- df
  } else {
    df2 <- df[grep(quotemeta(prod_up), df$product_name), ]
    df2 <- df2 %>% dplyr::mutate(product = product)
  }

  return(df2)

}

help_calc_distance <- function(long, lat, origin_long, origin_lat) {

  x <- abs(lat - origin_lat)
  y <- abs(long - origin_long)
  dist <- sqrt((x^2) + (y^2))
  out <- data.frame(long = long, lat = lat, dist = dist)

  return(out)

}


# for vignette
# source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
