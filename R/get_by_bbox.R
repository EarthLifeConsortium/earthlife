#' @title Return occurrences for all taxa within a bounding box.
#' @description A wrapper for the Composite API, returning all records from both databases.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#' @importFrom dplyr bind_rows
#' @param x A bounding box of format \code{c(lngW,latS,lngE,latN)}.
#' @param ... Other parameters to be passed into the API, described at \url{https://training.paleobiodb.org/comp1.0}.
#'
#' @author Simon J. Goring \email{goring@@wisc.edu}
#' @return More details on the use of these parameters can be obtained from
#'    \url{https://training.paleobiodb.org/comp1.0/}.
#'
#'    A list of class `occurrence` and `list`.  The list is composed of two elements:
#'
#'  \item{ \code{records} }{The complete listing of taxon occurrences.}
#'  \item{ \code{meta}  }{Metadata for the search.}
#'
#'  The \code{records} object is a \code{data.frame}
#'  \item{ \code{collection_name}  }{Site or collection unit name for the record.}
#'  \item{ \code{lng}  }{Collection site longitude.}
#'  \item{ \code{lat}  }{Collection site latitude.}
#'  \item{ \code{accepted_name}  }{The taxon name.}
#'  \item{ \code{max_age}  }{The oldest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{ \code{min_age}  }{The youngest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{ \code{age_unit}  }{The units for age (by default "Mya").}
#'  \item{ \code{database}  }{The database from which the record was obtained.}
#'  \item{ \code{occurrence_no}  }{The numeric ID for the record within the parent database.}
#'  \item{ \code{dataset_no}  }{For records within Neotoma, the numeric ID of the dataset from which the sample was obtained.}
#'  \item{ \code{accepted_no}  }{The numeric identifier for the taxon name from the parent database.}
#'  \item{ \code{collection_no}  }{The numeric identifier for the collection within the parent database.}
#'  \item{ \code{country}  }{The country within which the sample is found (if known).}
#'  \item{ \code{state}  }{The state (when known) within the identified country.}
#'
#' @examples \dontrun{
#' # Search for sites with "Canis" fossils.
#' pnw_search <- get_by_bbox(bbox = c(-140, 48, -110, 60), match_name = "Cupressaceae")
#'
#' # Limit searches to North America (undocumented use of \code{bbox})
#' canis_na <- get_by_bbox(x = c(-180, 20, -20, 90), match_name = "Canis")
#'
#' }
#' @references
#' EarthLife Consortium: http://earthlifeconsortium.org/
#' API Reference:  https://training.paleobiodb.org/comp1.0
#' @keywords IO connection
#' @export
#'
get_by_bbox <- function(x, ...) {
  UseMethod('get_by_bbox')
}

#' @export
get_by_bbox.default <- function(x, ...) {

  base_uri <- "https://training.paleobiodb.org/comp1.0/occs/list.json"

  ## Build the URL for the call.  This assumes only taxon matching.

  params <- as.list(match.call())
  params[[1]] <- NULL
  params <- lapply(params, eval, envir = parent.frame())

  params$bbox <- paste(params$x, collapse = ",")

  params$x <- NULL

  params$vocab <- "com"
  params$datainfo <- TRUE
  params$show <- "loc"
  params$ageunit <- "ma"

  neotoma_content <- httr::content(httr::GET(base_uri, query = params))

  records <- data.frame(do.call(dplyr::bind_rows, neotoma_content$records))
  colnames(records) <- earthlife:::record_cols$pbdb[match(colnames(records), record_cols$com)]

  if ("dataset_no" %in% colnames(records)) {

    # Resorting and excluding "record_type"
    col_names <- c("collection_name", "lng", "lat", "accepted_name",
                   "max_age", "min_age", "age_unit",
                   "database", "occurrence_no", "dataset_no", "accepted_no",
                   "collection_no", "country", "state")

    for (i in col_names[!"collection_name" %in% colnames(records)]) {
      records[,i] <- NA
    }

  } else {

    # This happens when only PBDB records are returned.

    records <- records[, c("collection_name", "lng", "lat", "accepted_name",
                           "max_age", "min_age", "age_unit",
                           "database", "occurrence_no", "accepted_no",
                           "collection_no", "country", "state")]

  }

  occurrence <- list(records = records,
                     meta = list(title = neotoma_content$title,
                                 access = as.POSIXct(neotoma_content$access_time,
                                                     format = "%a %F %T", tz = "GMT"),
                                 doc_url = neotoma_content$documentation_url,
                                 call = neotoma_content$data_url))

  class(occurrence) <- c("occurrence", "list")

  return(occurrence)
}
