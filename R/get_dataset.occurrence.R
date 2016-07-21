#' @export
get_dataset <- function(object,...) { UseMethod("get_dataset") }

#' @importFrom neotoma get_dataset
#' @export get_dataset.occurrence
#' @method get_dataset occurrence
get_dataset.occurrence <- function(x) {

  if ("dataset_no" %in% colnames(x$records)) {

    dataset_ids <- as.numeric(gsub(".*:", "", x$records$dataset_no, perl = TRUE))
    datasets <- neotoma::get_dataset(x = as.numeric(na.omit(dataset_ids)))

  } else {
    stop("This record contains no Neotoma dataset information.")
  }

  return(dataset)

}
