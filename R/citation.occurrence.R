#' @export
citation.occurrence <- function(package) {

  # We need to use package as the input parameter to make use of the `citation` alias:

  x <- package

  get_citation <- function(x) {
    paste0("  EarthLife Consortium. ", format(x$access, "%Y"),". ", x$title, ". ",
           "Access Date: ", format(x$access, "%F"), ". URL: ", x$call, "\n\n",
           "  @misc{ELC_", format(x$access, '%F'), ",\n",
           "    author={EarthLife Consortium},\n",
           "    title={",    x$title, "},\n",
           "    year={",     format(x$access, "%Y"), "},\n",
           "    urldate={",  format(x$access, "%F"), "},\n",
           "    url={",x$call, '}\n\t}')
  }

  if (!"access" %in% names(x$meta)) {
    output <- (sapply(occ_output$meta, get_citation))
  } else {
    output <- (get_citation(occ_output$meta))
  }

  cat("To cite this 'occurrence' object in publication, or to share its content use:\n\n")
  cat(output)
}
