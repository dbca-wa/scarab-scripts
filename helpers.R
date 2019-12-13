# System packages:
# sudo apt install libcairo2-dev libgdal-dev libudunits2-dev libjq-dev libv8-dev libprotobuf-dev protobuf-compiler

# remotes::install_github("ropenscilabs/skimr")
# remotes::install_github("tidyverse/readr")
# remotes::install_github("brianb/mdbtools")
# remotes::install_github("ropensci/ckanr")
# remotes::install_github('thomasp85/gganimate')
# remotes::install_github("ropensci/rredlist")
# remotes::install_github("ropensci/jqr")
# remotes::install_github("dbca-wa/wastdr")
# remotes::install_github("ropensci/ckanr")
# install.packages("knitr")
# install.packages("sf")
# install.packages("styler")
# install.packages("jsonlite")
# devtools::install_github("glin/reactable")

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
# library(sf)
library(jsonlite)
library(geojsonio)
library(geojsonsf)
library(mapview)
library(maps)
library(mapproj)
library(stringr)

library(gganimate)
library(ggthemes)
library(grDevices)

library(reactable)

# library(gapminder)

# Configure ckanr to data.dpaw.wa.gov.au
# usethis::edit_r_environ()
ckanr::ckanr_setup(url = Sys.getenv("CKAN_URL"), key = Sys.getenv("CKAN_API_KEY"))

# Date conventions
orders <- c("mdyHMS")
tz <- "Australia/Perth"
default_date <-
parse_as_datetime <- function(x){
    if (is.null(x)) {
        return(
            lubridate::parse_date_time(
                "1900-01-01 00:00:00", orders = "ymd HMS", tz = tz)
        )
        }
    x %>%
        lubridate::parse_date_time2(orders, tz = tz, cutoff_2000 = 18L) %>%
        lubridate::as_datetime(.) %>%
        lubridate::with_tz(tzone = tz)
}

dev <- "http://localhost:8220/api/1/"
uat <- "https://tsc-uat.dbca.wa.gov.au/api/1/"
prod <- "https://tsc.dbca.wa.gov.au/api/1/"

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
                      destdir=here::here("data"),
                      dateformat = "%m-%d-%Y",
                      as.is = TRUE,
                      verbose=TRUE) {
    if (!fs::dir_exists(destdir)) {fs::dir_create(destdir)}

    r <- ckanr::resource_show(resource_id)
    res_url <- r$url
    res_fn <- r$url %>% fs::path_file()
    res_file <- fs::path(fs::path(destdir, res_fn))

    if (!fs::file_exists(res_file)){
        if (verbose==TRUE) {
            message(
                glue::glue("Downloading {r$name} from CKAN to {res_file}..."))
            }
        utils::download.file(res_url, res_file)
    } else {
        if (verbose==TRUE) {
        message(glue::glue("Keeping already downloaded file {res_fn}.\n",
                           "Delete {res_fn} to force fresh download."))
        }
    }

    if (verbose==TRUE) {message(glue::glue("Extracting {res_file}..."))}
    dbfile <- utils::unzip(res_file, exdir = destdir)
    con <- Hmisc::mdb.get(dbfile, dateformat = dateformat, as.is = as.is)
    if (verbose==TRUE) {message("Done, returning open db connection.")}
    con
}

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
load_ckan_csv <- .  %>% resource_show() %>% magrittr::extract2("url") %>% read_csv()



chunk_post <- function(data, serializer = "names", api_url = wastdr::get_wastdr_api_url(),
          api_token = wastdr::get_wastdr_api_token(), api_un = wastdr::get_wastdr_api_un(),
          api_pw = wastdr::get_wastdr_api_pw(), chunksize = 1000, verbose = FALSE) {
    . <- NULL
    if (verbose) message("[chunk_post] Updating ", api_url, serializer, "...")
    len <- nrow(data)
    for (i in 0:((len/chunksize)-1)) {
        start <- (i * chunksize) + 1
        end <- min((start + chunksize) - 1, len)
        message("[chunk_post] Processing feature ", start, " to ", end)
        data[start:end,] %>%
            wastdr::wastd_POST(., serializer = serializer, api_url = api_url,
                       api_token = api_token, api_un = api_un, api_pw = api_pw,
                       verbose = verbose)
    }
    message("[chunk_post] Finished, ", len, " records created/updated.")
}

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
    data %>%
        sf::st_as_sf(coords = lonlat_cols, crs = crs, agr = "constant") %>%
        sf::st_union(.) %>%
        sf::st_convex_hull(.) %>%
        sf::st_buffer(., 0.00001)
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

