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
# remotes::install_github("dbca-wa/wastdr", lib="/home/milly/R/library")
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

# -----------------------------------------------------------------------------#
# Server shortcuts
dev <- "http://localhost:8220/api/1/"
uat <- "https://tsc-uat.dbca.wa.gov.au/api/1/"
prod <- "https://tsc.dbca.wa.gov.au/api/1/"
dev_token <- Sys.getenv("WASTDR_API_DEV_TOKEN", unset = "")

# -----------------------------------------------------------------------------#
# Setup packages
#
# Configure ckanr to data.dpaw.wa.gov.au
# usethis::edit_r_environ()
ckanr::ckanr_setup(url = Sys.getenv("CKAN_URL"), key = Sys.getenv("CKAN_API_KEY"))


# -----------------------------------------------------------------------------#
# Web helpers
#
#' Download, extract and open a zipped Access database from a CKAN dataset.
#'
#' The extracted file will be kept in `destdir`.
#' If a file with the expected filename already exists in `destdir`, this file
#' will be used.
#' To force a fresh download, remove or rename the file in `destdir`.
#'
#' @param resource_id The CKAN resource ID of a zipped Access DB.
#' @param destdir The local destination directory for the extracted file,
#'  will be created if not existing. Default: `here::here("data")`.
#' @param dateformat The parameter dateformat for `Hmisc::mdb.get()`,
#'   default: `%Y-%m-%d`.
#' @param as.is The parameter `as.is` for `Hmisc::mdb.get()`, default: TRUE.
#' @returns The `Hmisc::mdb.get` connection.
dl_mdbzip <- function(resource_id,
                      destdir = here::here("data"),
                      dateformat = "%m-%d-%Y",
                      as.is = TRUE,
                      verbose = wastdr::get_wastdr_verbose()) {
    if (!fs::dir_exists(destdir)) {fs::dir_create(destdir)}

    r <- ckanr::resource_show(resource_id)
    res_url <- r$url
    res_fn <- r$url %>% fs::path_file()
    res_file <- fs::path(fs::path(destdir, res_fn))

    if (!fs::file_exists(res_file)){
        if (verbose==TRUE)
            wastdr::wastdr_msg_info(
                glue::glue("Downloading {r$name} from CKAN to {res_file}...")
            )
        utils::download.file(res_url, res_file)
    } else {
        if (verbose==TRUE)
            wastdr::wastdr_msg_noop(
                glue::glue("Keeping already downloaded file {res_fn}. ",
                           "Delete {res_fn} to force fresh download.")
            )

    }

    if (verbose==TRUE)
        wastdr::wastdr_msg_info(glue::glue("Extracting {res_file}..."))
    dbfile <- utils::unzip(res_file, exdir = destdir)
    con <- Hmisc::mdb.get(dbfile, dateformat = dateformat, as.is = as.is)
    if (verbose==TRUE)
        wastdr::wastdr_msg_success("Done, returning open db connection.")
    con
}

#' Create or update a CKAN resource.
#'
#'
#' @details The data will be written to CSV in a directory `data/` with the resource title
#'   in snake_case. If no resource ID is given, a resource will be created.
#'   The resource ID is returned in either case.
#' @param data A data frame to write to disk
#' @param resource_title A CKAN resource title
#' @param dataset_id A CKAN dataset (package) ID
#' @param resource_id A CKAN resource ID, default: NULL
#' @return The resource ID of the created or updated resource.
#' @examples
#' \notrun{
#' d <- ckanr::package_show("threatened-ecological-communities-database")
#'
#' # Run this once to create resource and retrieve resource ID
#' upload_to_ckan(a_tibble, "Resource title", d$id, resource_id = NULL)
#' # returns "502c74d7-32be-453f-aff6-c50aedd3deed" - paste into resource_id
#'
#' # Re-run this to update resource with new data
#' upload_to_ckan(a_tibble, "Resource title", d$id, resource_id = "502c74d7-32be-453f-aff6-c50aedd3deed")
#' }
upload_to_ckan <- function(data,
                           resource_title,
                           dataset_id,
                           resource_id=NULL){

    resource_filename <- resource_title %>%
        stringr::str_to_lower(.) %>%
        stringr::str_replace_all(., " ", "_") %>%
        paste0(".csv") %>%
        file.path("data", .)

    write_delim(data, resource_filename, delim = ",")

    if (is.null(resource_id)){
        cat("No resource ID given, creating a new resource for", resource_title, ": ")
        r <- ckanr::resource_create(package_id = dataset_id,
                                    format = "csv",
                                    name = resource_title,
                                    upload = resource_filename)
    } else {
        cat("Updating CKAN resource", resource_title, ": ")
        r <- ckanr::resource_update(resource_id,
                                    resource_filename)

    }
    r$id
}

#' Upload a file to an existing CKAN resource ID. Skip if file missing.
upload_file_to_ckan <- function(rid, fn){
    if (fs::file_exists(fn)) {
        message(glue::glue("Uploading {fn} to data catalogue..."))
        r <- ckanr::resource_update(rid, fn)
        message(glue::glue("Updated {r$name} at\n{r$url}"))
    } else {
        message(glue::glue("File {fn} does not exist, skipping."))
    }
}


#' Load a CSV from a CKAN resource ID, requires ckanr_setup
load_ckan_csv <- .  %>%
    resource_show() %>%
    magrittr::extract2("url") %>%
    read_csv()


chunk_post <- function(data,
                       serializer = "names",
                       query = list(),
                       api_url = wastdr::get_wastdr_api_url(),
                       api_token = wastdr::get_wastdr_api_token(),
                       api_un = wastdr::get_wastdr_api_un(),
                       api_pw = wastdr::get_wastdr_api_pw(),
                       chunksize = 1000,
                       verbose = wastdr::get_wastdr_verbose()) {
    if (verbose)
        wastdr::wastdr_msg_info(
            glue::glue("[chunk_post][{Sys.time()}] Updating {api_url}{serializer}..."))
    len <- nrow(data)
    for (i in 0:((len/chunksize)-1)) {
        start <- (i * chunksize) + 1
        end <- min((start + chunksize) - 1, len)
        wastdr::wastdr_msg_info(
            glue::glue("[chunk_post][{Sys.time()}] Processing feature {start} to {end}"))
        data[start:end,] %>%
            wastdr::wastd_POST(.,
                               serializer = serializer,
                               query = query,
                               api_url = api_url,
                               api_token = api_token,
                               api_un = api_un,
                               api_pw = api_pw,
                               verbose = verbose)
    }
    wastdr::wastdr_msg_success(
        glue::glue("[chunk_post][{Sys.time()}] Finished, {len} records created/updated."))
}


#' Post a list of records to "occ-observation/bulk_create"
#'
#' @param data A tbl_df of occ-observation records
#' @param obstype The model type of the occ-observation model,
#'   default: "PhysicalSample"
#' @param api_url TSC API URL, default: `wastdr::get_wastdr_api_url()`
#' @param api_token TSC API URL, default: `wastdr::get_wastdr_api_token()`
#' @export
wastd_occ_obs_post <- function(data,
                               obstype = "PhysicalSample",
                               chunksize = 100,
                               api_url = wastdr::get_wastdr_api_url(),
                               api_token = wastdr::get_wastdr_api_token()) {
    wastdr::wastdr_msg_info(
        glue::glue("[{Sys.time()}] Uploading {nrow(data)} ",
                   "{obstype}s to  TSC {api_url}")
    )

    res <- chunk_post(
        data,
        serializer = "occ-observation/bulk_create",
        query = list(obstype = obstype),
        api_url = api_url,
        api_token = api_token,
        chunksize = chunksize
    )

    if ("created_count" %in% names(res$data))
        wastdr::wastdr_msg_success(
            glue::glue("[{Sys.time()}] Done, created ",
                       "{res$data$created_count} records."))
    if ("errors" %in% names(res$data) &&
        length(res$data$errors) > 0)
        wastdr::wastdr_msg_warn(
            glue::glue("[{Sys.time()}] Got {length(res$data$errors)} errors."))
    res
}

# -----------------------------------------------------------------------------#
# Convenience helpers
#
# Make column def (mkd)
add_dettol <- . %>% stringr::str_to_lower(.) %>% stringr::str_replace_all(., "\\.", "_")
as_def <- . %>% paste(add_dettol(.), "=", ., " %>% as.character, \n")
mkd <- . %>% names() %>% purrr::map(as_def) %>% unlist() %>% cat(.)

#' Normalise corporate filenumber into <agency>-<year>-<number> or ""
as_filenumber <- function(body, prefix="DBCA", sep="-") {
    if (is.null(body) || body == "") return("")
    urlized_body <- stringr::str_replace_all(body, "F|/", "-")
    paste0(prefix, sep, urlized_body)
}

#' Parse a string into a list of integer numbers
str_to_int_array <- . %>% strsplit(., ",")[[1]] %>% as.list

chr2int <- . %>%
    stringr::str_split(",") %>%
    purrr::map(as.integer)

# Date conventions
orders <- c("mdyHMS","dmy")
tz <- "Australia/Perth"
default_date <- "1900-01-01 00:00:00"
default_date_notime <- "1900-01-01"
parse_as_datetime <- function(
    x,
    default_datetime = lubridate::parse_date_time(
        "1900-01-01 00:00:00", orders = "ymd HMS", tz = tz)){
    if (is.null(x)) return(default_datetime)

    x %>%
        lubridate::parse_date_time2(orders, tz = tz, cutoff_2000 = 18L) %>%
        lubridate::as_datetime(.) %>%
        lubridate::with_tz(tzone = tz)
}

# Present tabular data as reactable
rt <- . %>%
    reactable::reactable(
        sortable = TRUE,
        filterable = TRUE,
        style = "color:black;"
    )


## Create occurence data tables for each card in TSC
has_name_id_and_location <- . %>%
    dplyr::filter(
        !is.na(name_id) & !is.na(latitude) & !is.na(longitude) & name_id != 0
    )

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
                                  lonlat_cols=c("longitude", "latitude"),
                                  crs=4283) {
    suppressWarnings(
        data %>%
            sf::st_as_sf(coords = lonlat_cols, crs = crs, agr = "constant") %>%
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
        dplyr::filter(
            !name_id %in% c(NA, 0),
            !is.na(longitude),
            !is.na(latitude)
        ) %>%
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
                        nid=NULL,
                        nid_col="name_id",
                        lonlat_cols=c("longitude", "latitude"),
                        crs=4283) {
    if (is.null(nid)) {
        selected <- data
    } else {
        selected <- dplyr::filter(data, (!! rlang::sym(nid_col)) == nid)
    }
    if (nrow(selected) == 0) return(NULL)
    lonlat_to_convex_hull(selected, lonlat_cols = lonlat_cols, crs = crs)
}

anim_fauna <- function(data, title, nid){
    d <- data %>%
        dplyr::mutate(year = lubridate::year(datetime)) %>%
        dplyr::filter(name_id == nid)

    ggplot() +
        borders("world", colour = "gray75", fill = "gray95") +
        coord_equal(xlim = c(110, 130), ylim = c(-40,-12)) +
        ggthemes::theme_map(base_size = 14) +
        ggtitle(title, subtitle = "{lubridate::year(frame_time)}") +
        geom_point(data = d,
                   aes(x = longitude,
                       y = latitude,
                       size = number_seen,
                       colour = observation_type),
                   alpha = 0.7,
                   show.legend = FALSE) +
        scale_size(range = c(2,8)) +
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
fix_incomplete_date <- function(
    date_string,
    default_date_notime =
        (lubridate::parse_date_time("01/01/1900 12:00:00+08",orders = "dmYHMSz", tz = "Australia/Perth"))) {
    if (date_string == "") {
        return(default_date_notime)
    }

    date_parts <- stringr::str_split(date_string, "/")
    day_int <- as.integer(date_parts[[1]][[1]])
    day <- ifelse(is.na(day_int), 1, day_int)
    month_int <- as.integer(date_parts[[1]][[2]])
    month <- ifelse(is.na(month_int), 1, month_int)
    year_int <- as.integer(date_parts[[1]][[3]])
    year <- ifelse(is.na(year_int), 1900, year_int)
    lubridate::make_datetime(year, month, day, 12, tz = tz)
}
