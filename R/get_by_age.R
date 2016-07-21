#' @title Return occurrences for all taxa within a specified time range.
#' @description A wrapper for the Composite API, returning all records from both databases.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#' @importFrom dplyr bind_rows
#'
#' @param x An (optional) age range \code{c(agemin, agemax)}.
#' @param ageunit The age units for the age range.  By default \code{"ma"} (millions of years ago), accepts \code{"kya"} (thousands of years ago) and \code{"ybp"} (years before present).
#' @param timerule Resolve time overlap using the appropriate rule: \code{contain} (records are fully within range), \code{major} (records at least 50\% within), \code{overlap} (records overlap), or \code{buffer} (fully contained, with a buffer of - default - 20\%).
#' @param timebuffer (optional) for use with \code{timerule == "buffer"}, the temporal buffer around the defined age range in \code{x}.
#' @param ... Other parameters to be passed into the API, described at \url{https://training.paleobiodb.org/comp1.0}.
#'
#' @author Simon J. Goring \email{goring@@wisc.edu}
#'
#' @return More details on the use of these parameters can be obtained from
#'  \url{https://training.paleobiodb.org/comp1.0/}.
#'
#'  A list of class `occurrence` and `list`.  The list is composed of two elements:
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
#' holocene_snap <- get_by_age(x = c(12000, 11500), ageunit = "ybp", timerule = "contain", base_name = "")
#'
#' # Limit searches to North America (undocumented use of \code{bbox})
#'
#' }
#' @references
#' EarthLife Consortium: http://earthlifeconsortium.org/
#' API Reference:  https://training.paleobiodb.org/comp1.0
#' @keywords IO connection
#' @export
#'
get_by_age <- function(x, ageunit = "Mya", timerule = "contain", timebuffer = "20", ...) {
  UseMethod('get_by_age')
}

#' @export
get_by_age.default <- function(x, ageunit = "Mya", timerule = "contain", timebuffer = "20", ...) {

  params <- as.list(match.call())
  params[[1]] <- NULL
  params <- lapply(params, eval, envir = parent.frame())

  if (!any(c('occ_id', 'taxon_name', 'base_name', 'match_name', 'base_id', 'taxon_id', 'site_id', 'bbox') %in% names(params))) {
    stop('')
  }

  base_uri <- "https://training.paleobiodb.org/comp1.0/occs/list.json"

  ## Assertions:
  age_test <- assertthat::assert_that(params$ageunit %in% c("ma", "kya", "ybp"))
  if (!age_test) {
    stop("'timeunit' must be one of 'ma', 'kya' or 'ybp'")
  }
  age_rule <- assertthat::assert_that(params$timerule %in% c("contain", "major", "overlap", "buffer"))
  if (!age_rule) {
    stop("'timerule' must be one of 'contain', 'major', 'overlap', or 'buffer'")
  }

  if (ageunit == "Mya") {
    params$min_ma <- min(params$x)
    params$max_ma <- max(params$x)
    params$ageunit <- "ma"
  } else {
    if (ageunit == "kya") {
      params$min_age <- min(params$x) * 1000
      params$max_age <- max(params$x) * 1000
      params$ageunit <- "ybp"
    } else {
      params$min_age <- min(params$x)
      params$max_age <- max(params$x)
      params$ageunit <- "ybp"
    }
  }

  params$x <- NULL

  params$vocab = "com"

  if ('bbox' %in% params) {
    params$bbox <- paste(params$bbox, collapse = ",")
  }

  neotoma_content <- httr::content(httr::GET(base_uri, query = params))

  if ('warnings' %in% names(neotoma_content)) {
    lapply(neotoma_content$warnings, warning)
  }

  records <- data.frame(do.call(dplyr::bind_rows, neotoma_content$records))

  colnames(records) <- record_cols$pbdb[match(colnames(records), record_cols$com)]

  if ("dataset_no" %in% colnames(records)) {

    # Resorting and excluding "record_type"

    records <- records[, c("collection_name", "lng", "lat", "accepted_name",
                           "max_age", "min_age", "age_unit",
                           "database", "occurrence_no", "dataset_no", "accepted_no",
                           "collection_no", "country", "state")]
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
