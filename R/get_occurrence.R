#' @title Return occurrences for taxa within the PBDB/Neotoma.
#' @description A wrapper for the Composite API, returning all records from both datasets.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#' @param x An optional value, either a \code{numeric} site ID or object of class \code{download}, \code{download_list} or \code{site}.
#' @param occ_id
#' @param taxon_name
#' @param base_name
#' @param match_name
#' @param vocab The taxonomic vocabulary to use, either \'pbdb\' or \'neotoma\'.
#' @param base_id
#' @param taxon_id
#' @param site_id
#' @param bbox
#'
#' @author Simon J. Goring \email{goring@@wisc.edu}
#' @return More details on the use of these parameters can be obtained from
#'    \url{https://training.paleobiodb.org/comp1.0/}.
#'
#'    A list of class `data.frame`.  , with each item corresponding to an individual record.
#'    Searches that return no items will result in a NULL value being returned.
#'    Otherwise each list item (each dataset record) includes the following components:
#'
#'  \item{ \code{dataset.id} }{Unique database record identifier for the dataset.}
#'  \item{ \code{dataset.name}  }{Name of the dataset; not commonly used.}
#'  \item{ \code{CollUnitHandle}  }{Code name of the Collection Unit with which the dataset is associated. This code may be up to 10 characters. Data are frequently distributed by Collection Unit, and the Handle is used for file names.}
#'  \item{ \code{CollUnitID}  }{Unique database record identifier for the collection unit.}
#'  \item{ \code{CollType}  }{The collection type. Types include cores, sections, excavations, and animal middens.}
#'  \item{ \code{DatasetType}  }{The dataset type, such as: geochronologic, loss-on-ignition, pollen, plant macrofossils, vertebrate fauna, etc.}
#'  \item{ \code{AgeOldest}  }{The oldest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{ \code{AgeYoungest}  }{The youngest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{ \code{SubDates}  }{An array of objects that describe dataset submission events.  If multiple submissions occurred then this is a table.}
#'  \item{ \code{DatasetPIs}  }{An array of objects that describe Principal Investigators associated with a dataset.}
#'  \item{ \code{Site}  }{An object describing the site where the dataset samples were taken.}
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
