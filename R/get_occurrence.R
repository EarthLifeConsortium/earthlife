#' @title Return occurrences for taxa within the PBDB/Neotoma.
#' @description A wrapper for the Composite API, returning all records from both datasets.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#' @param x An optional value, either a \code{numeric} site ID or object of class \code{download}, \code{download_list} or \code{site}.
#' @param occ_id A numeric Occurrence ID.
#' @param taxon_name The taxon name as a \code{character}
#' @param base_name The taxon name as a \code{character}
#' @param match_name The taxon name as a \code{character}
#' @param vocab The taxonomic vocabulary to use, either \'pbdb\' or \'neotoma\'.
#' @param base_id The taxon name as a \code{character}
#' @param taxon_id The taxon name as a \code{character}
#' @param site_id The taxon name as a \code{character}
#' @param bbox The taxon name as a \code{character}
#'
#' @author Simon J. Goring \email{goring@@wisc.edu}
#' @return More details on the use of these parameters can be obtained from
#'    \url{https://training.paleobiodb.org/comp1.0/}.
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
#' # Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' # that are on the west coast of North America:
#' t8kyr.datasets <- get_dataset(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' # Search for vertebrate fossils in Canada (gpid: 756) within the last 2kyr.
#' gpids <- get_table(table.name='GeoPoliticalUnits')
#' canID <- gpids[which(gpids$GeoPoliticalName == 'Canada'),1]
#'
#' v2kyr.datasets <- get_dataset(datasettype='vertebrate fauna', gpid=canID, ageold = 2000)
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  https://training.paleobiodb.org/comp1.0
#' @keywords IO connection
#' @export
#'
get_occurrence <- function(x, occ_id, taxon_name, base_name, match_name, base_id, taxon_id, site_id, bbox, vocab) {
  UseMethod('get_occurrence')
}

get_occurrence.default <- function(x, occ_id, taxon_name, base_name, match_name, base_id, taxon_id, site_id, bbox, vocab) {

  base_uri <- 'https://training.paleobiodb.org/comp1.0/occs/'

  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())

  if ("base_name" %in% names(cl) & !"vocab" %in% names(cl)) {
    stop("You must supply a vocabulary for use with \`base_names\`.  Use the \`vocab\` parameter.")
  }

  if ("base_name" %in% names(cl)) {
    base_uri <- paste0(base_uri, "list.txt")
  }

  neotoma_content <- textConnection(httr::content(httr::GET(base_uri, query = cl), as = 'text'))

  if (regexpr("list.txt", base_uri) > 0) {
    return(read.csv(neotoma_content))
  }

  return(neotoma_content)
}
