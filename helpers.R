# -----------------------------------------------------------------------------#
# RStudio Server
#
# System packages provided by https://rstudio.dbca.wa.gov.au/
# sudo apt install libcairo2-dev libgdal-dev libudunits2-dev libjq-dev libv8-dev \
# libprotobuf-dev protobuf-compiler libavfilter-dev libfontconfig1-dev

# !!! Install system packages on Rstudio Server !!!
# sudo apt install mdbtools cargo libavfilter-dev libfontconfig1-dev

# -----------------------------------------------------------------------------#
# Package installation
#
# R packages provided by https://rstudio.dbca.wa.gov.au/
# remotes::install_github("ropenscilabs/skimr")
# remotes::install_github("tidyverse/readr")
# remotes::install_github("ropensci/ckanr")
# remotes::install_github('thomasp85/gganimate')
# remotes::install_github("ropensci/jqr")
# install.packages("knitr")
# install.packages("sf")
# install.packages("styler")
# install.packages("jsonlite")
# devtools::install_github("glin/reactable")
# remotes::install_github("r-lib/lifecycle")
# remotes::install_github("ropensci/rredlist")
# remotes::install_github("dbca-wa/wastdr")
# remotes::install_github("dbca-wa/wastdr", dependencies = TRUE, lib="/home/milly/R/library")
# remotes::install_github("dbca-wa/wastdr", lib="/home/florianm/R/library")

# -----------------------------------------------------------------------------#
# Load packages
#
library(tidyverse)

library(devtools)
library(usethis)
library(lubridate)
library(skimr)
library(Hmisc)
library(ckanr)
library(glue)
library(knitr)
library(styler)
library(janitor)
library(wastdr)
# library(rstan)
library(leaflet)
# library(RColorBrewer)
library(sf)
library(jsonlite)
library(geojsonio)
library(geojsonsf)
library(mapview)
library(maps)
library(mapproj)
library(stringr)
library(magrittr)
library(gganimate)
library(gifski)
library(av)
library(ggthemes)
library(grDevices)
library(reactable)

library(tscr)

# -----------------------------------------------------------------------------#
# Server shortcuts
dev <- "http://localhost:8220/api/1/"
uat <- "https://tsc-uat.dbca.wa.gov.au/api/1/"
prod <- "https://tsc.dbca.wa.gov.au/api/1/"
dev_token <- Sys.getenv("TSC_API_DEV_TOKEN", unset = "")

# -----------------------------------------------------------------------------#
# Setup packages
#
# Configure ckanr to data.dpaw.wa.gov.au
# usethis::edit_r_environ()
ckanr::ckanr_setup(url = Sys.getenv("CKAN_URL"), key = Sys.getenv("CKAN_API_KEY"))


# -----------------------------------------------------------------------------#
# Web helpers
# tscr::dl_mdbzip()
# tscr::upload_to_ckan()
# tscr::upload_file_to_ckan()

#' Load a CSV from a CKAN resource ID, requires ckanr_setup
load_ckan_csv <- .  %>%
    resource_show() %>%
    magrittr::extract2("url") %>%
    read_csv()

# -----------------------------------------------------------------------------#
# Convenience helpers
#
# Make column def (mkd)
add_dettol <-
    . %>% stringr::str_to_lower(.) %>% stringr::str_replace_all(., "\\.", "_")
as_def <-
    . %>% paste(add_dettol(.), "=", ., " %>% as.character, \n")
mkd <- . %>% names() %>% purrr::map(as_def) %>% unlist() %>% cat(.)

#' Normalise corporate filenumber into <agency>-<year>-<number> or ""
as_filenumber <- function(body, prefix = "DBCA", sep = "-") {
    if (is.null(body) || body == "")
        return("")
    urlized_body <- stringr::str_replace_all(body, "F|/", "-")
    paste0(prefix, sep, urlized_body)
}

#' Parse a string into a list of integer numbers
str_to_int_array <- . %>% strsplit(., ",")[[1]] %>% as.list

chr2int <- . %>%
    stringr::str_split(",") %>%
    purrr::map(as.integer)

# Date conventions
orders <- c("mdyHMS", "dmy")
tz <- "Australia/Perth"
default_date <- "1900-01-01 00:00:00"
default_date_notime <- "1900-01-01"
parse_as_datetime <- function(x,
                              default_datetime = lubridate::parse_date_time("1900-01-01 00:00:00", orders = "ymd HMS", tz = tz)) {
    if (is.null(x))
        return(default_datetime)

    x %>%
        lubridate::parse_date_time2(orders, tz = tz, cutoff_2000 = 20L) %>%
        lubridate::as_datetime(.) %>%
        lubridate::with_tz(tzone = tz)
}

# Present tabular data as reactable
rt <- . %>%
    reactable::reactable(sortable = TRUE,
                         filterable = TRUE,
                         style = "color:black;")


## Create occurence data tables for each card in TSC
has_name_id_and_location <- . %>%
    dplyr::filter(
        !is.na(name_id) &
            !is.na(latitude) &
            !is.na(longitude) &
            name_id != 0 &
            latitude >= -90 &
            latitude <= 90 &
            longitude >= -180 &
            longitude <= 180
    )

impossible_location <- . %>%
    dplyr::filter(latitude < -90 |
                      latitude > 90 |
                      longitude < -180 |
                      longitude > 180)

# -----------------------------------------------------------------------------#
# Spatial helpers
#
#' Return the convex hull plus minimal buffer around all given points.
#'
#' A minimal buffer is included to turn single point observations
#' into actual polygons.
#' @param lonlat_cols A vector of lon and lat column names,
#'   default: c("longitude", "latitude")
#' @param crs The coordinate reference system ID (4283=GDA94)
#' @return An object of class "sfc_POLYGON" "sfc" (package sf)
lonlat_to_convex_hull <- function(data,
                                  lonlat_cols = c("longitude", "latitude"),
                                  crs = 4283) {
    suppressWarnings(
        data %>%
            sf::st_as_sf(
                coords = lonlat_cols,
                crs = crs,
                agr = "constant"
            ) %>%
            sf::st_union(.) %>%
            sf::st_convex_hull(.) %>%
            sf::st_buffer(., 0.00001)
    )
}

#' Convert occurrence records into a dataframe of distinct name_id:eoo
#'
#' **Data** must have the columns `name_id`, `latitude`, and `longitude`.
#' **Filter** out occurrences of invalid NameIDs (NA and 0).
#' **Nest** occurrence data by name_id to generate two columns,
#'   `name_id` and `data` (all records of that `name_id`).
#' **Map** `lonlat_to_convex_hull` over data column to generate convex
#'   hulls (class `sfc`) from all points in `data` column.
#' **Map** `sfc_geojson` to convert the convex hulls into GeoJSON.
#' @param data A dataframe with at least the columns
#'   `name_id`, `latitude`, and `longitude`.
#' @return A dataframe with columns `name_id`, `data`, `eeo_sfc`, and `eeo`:
#'      name_id: one row per distinct name_id in input dataframe.
#'      data: the nested data of all rows with that name_id.
#'      eeo_sfc: the extent of occurrence as object of class `sfc`.
#'      eeo: the extent of occurrence as GeoJSON string.
#' @examples
#'   \notrun{
#'   data %>% make_eoo %>% mapview(.$eoo_sfc)
#'   data %>% make_eoo %>% dplyr::select(-data, -eoo_sfc) %>% wastdr::wastd_POST("taxon")
#'   }
make_eoo <- function(data) {
    data %>%
        dplyr::filter(!name_id %in% c(NA, 0),!is.na(longitude),!is.na(latitude)) %>%
        tidyr::nest(-name_id) %>%
        dplyr::mutate(
            eoo_sfc = purrr::map(data, lonlat_to_convex_hull),
            eoo = purrr::map(eoo_sfc, geojsonsf::sfc_geojson)
        )
}

#' Return the convex hull around all points with given name_id
#'
#' @param data A tibble or data.frame with columns
#'   `longitude`, `latitude`, `name_id`
#' @param nid A numeric name_id, default: null (use all rows)
#' @param nid_col The column name of the NameID column, default: "name_id"
#' @param lonlat_cols A vector of lon and lat column names,
#'   default: c("longitude", "latitude")
#' @param crs The coordinate reference system ID (4283=GDA94)
#' @return An object of class "sfc_POLYGON" "sfc" (package sf)
#'   if points are given, else NULL.
eoo_polygon <- function(data,
                        nid = NULL,
                        nid_col = "name_id",
                        lonlat_cols = c("longitude", "latitude"),
                        crs = 4283) {
    if (is.null(nid)) {
        selected <- data
    } else {
        selected <- dplyr::filter(data, (!!rlang::sym(nid_col)) == nid)
    }
    if (nrow(selected) == 0)
        return(NULL)
    lonlat_to_convex_hull(selected, lonlat_cols = lonlat_cols, crs = crs)
}

anim_fauna <- function(data, title, nid) {
    d <- data %>%
        dplyr::mutate(year = lubridate::year(datetime)) %>%
        dplyr::filter(name_id == nid)

    ggplot() +
        borders("world", colour = "gray75", fill = "gray95") +
        coord_equal(xlim = c(110, 130), ylim = c(-40, -12)) +
        ggthemes::theme_map(base_size = 14) +
        ggtitle(title, subtitle = "{lubridate::year(frame_time)}") +
        geom_point(
            data = d,
            aes(
                x = longitude,
                y = latitude,
                size = number_seen,
                colour = observation_type
            ),
            alpha = 0.7,
            show.legend = FALSE
        ) +
        scale_size(range = c(2, 8)) +
        transition_time(o_date) +
        ease_aes('linear')

    # anim_save(glue::glue("data/occ_{nid}.gif"))
}

#' Fix an incomplete date string and return a valid date
#'
#' @details If given an empty string, the default date string is returned.
#' If given an incomplete date string, missing days and months are backfilled
#' with "01", missing years are filled with "1900".
#' If given a valid date string, it is returned as is.
#' If given a date string in any format other than "dmY", XXX happens.
#' @param date_string <chr> A string of format "dd/mm/yyyy" possibly with
#'   missing day, month, or year
#' @param default_date_notime <chr> A string "01/01/1900"
#' @return A valid date as string
#' @examples
#' \dontrun{
#' testthat::expect_equal(fix_incomplete_date("02/03/2015"),
#'                       lubridate::parse_date_time("02/03/2015 12:00:00+08",orders = "dmYHMSz", tz = "Australia/Perth"))
#'testthat::expect_equal(fix_incomplete_date(" /03/2015"),
#'                       lubridate::parse_date_time("01/03/2015 12:00:00+08",orders = "dmYHMSz", tz = "Australia/Perth"))
#'testthat::expect_equal(fix_incomplete_date(" / /2015"),
#'                       lubridate::parse_date_time("01/01/2015 12:00:00+08",orders = "dmYHMSz", tz = "Australia/Perth"))
#'testthat::expect_equal(fix_incomplete_date(""),
#'                       lubridate::parse_date_time("01/01/1900 12:00:00+08",orders = "dmYHMSz", tz = "Australia/Perth"))
#' }
fix_incomplete_date <-
    function(date_string,
             default_date_notime = (
                 lubridate::parse_date_time(
                     "01/01/1900 12:00:00+08",
                     orders = "dmYHMSz",
                     tz = "Australia/Perth")
                 )
             ) {
        if (date_string == "") {return(default_date_notime)}

        date_parts <- stringr::str_split(date_string, "/")
        day_int <- as.integer(date_parts[[1]][[1]])
        day <- ifelse(is.na(day_int), 1, day_int)
        month_int <- as.integer(date_parts[[1]][[2]])
        month <- ifelse(is.na(month_int), 1, month_int)
        year_int <- as.integer(date_parts[[1]][[3]])
        year <- ifelse(is.na(year_int), 1900, year_int)
        lubridate::make_datetime(year, month, day, 12, tz = tz)
    }
