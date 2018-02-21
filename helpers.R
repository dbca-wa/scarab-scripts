#' Download, extract and open a zipped Access database from a CKAN dataset
#'
#' @param resource_id The CKAN resource ID of a zipped Access DB
#' @param destdir The local destination directory for the extracted file,
#'  will be created if not existing. Default: "data"
#' @param dateformat The parameter dateformat for Hmisc::mdb.get(), default: '%Y-%m-%d'
#' @param asis The parameter as.is for Hmisc::mdb.get(), default: TRUE
#' @returns The Hmisc::mdb.get connection
dl_mdbzip <- function(resource_id,
                      destdir="data",
                      dateformat = "%m-%d-%Y",
                      asis = TRUE) {
    dir.create(file.path(getwd(), destdir), showWarnings = FALSE)
    tmp <- tempfile()
    res_url <- ckanr::resource_show(resource_id)$url
    utils::download.file(res_url, tmp)
    dbfile <- utils::unzip(tmp, exdir = destdir)
    con <- Hmisc::mdb.get(dbfile, dateformat = dateformat, as.is = asis)
    unlink(tmp)
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