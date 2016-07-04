#' @title Return occurrences for taxa within the PBDB/Neotoma.
#' @description A wrapper for the Composite API, returning all records from both datasets.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#' @importFrom dplyr bind_rows
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
get_by_taxon <- function(x, lower = TRUE, pattern = TRUE) {
  UseMethod('get_by_taxon')
}

get_by_taxon.default <- function(x, lower = TRUE, pattern = TRUE) {

  base_uri <- "https://training.paleobiodb.org/comp1.0/occs/list.json"

  ## Build the URL for the call.  This assumes only taxon matching.

  if (lower == TRUE) {
    if (pattern == TRUE) {
      params <- list(match_name = x, vocab = "com", datainfo = TRUE, show = "loc", ageunit = "ma")
    } else {
      params <- list(base_name = x, vocab = "com", datainfo = TRUE, show = "loc", ageunit = "ma")
    }
  } else {
      params <- list(taxon_name = x, vocab = "com", datainfo = TRUE, show = "loc", ageunit = "ma")
  }


  neotoma_content <- httr::content(httr::GET(base_uri, query = params))

  occurrence <- list(records = data.frame(do.call(bind_rows, neotoma_content$records)),
                     meta = list(title = neotoma_content$title,
                                 access = as.POSIXct(neotoma_content$access_time,
                                                     format = "%a %F %T", tz = "GMT"),
                                 doc_url = neotoma_content$documentation_url,
                                 call = neotoma_content$data_url))

  class(occurrence) <- c("occurrence", "list")

  return(occurrence)
}
