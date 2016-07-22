#' @export
citation.occurrence <- function(package) {

  # We need to use package as the input parameter to make use of the `citation` alias:

  x <- package
  citation <- paste0("To cite this 'occurrence' object in publication, or to share its content use:\n",
                     "\n\n",
                     "  EarthLife Consortium.", format(x$meta$access, "%Y"),". ", x$meta$title, ". ",
                     "Access Date: ", format(x$meta$access, "%F"), ". URL: ", x$meta$call, "\n\n",
                     "  @misc{ELC_", format(x$meta$access, '%F'), ",\n",
                     "    author={EarthLife Consortium},\n",
                     "    title={",    x$meta$title, "},\n",
                     "    year={",     format(x$meta$access, "%Y"), "},\n",
                     "    urldate={",  format(x$meta$access, "%F"), "},\n",
                     "    url={",x$meta$call, '}\n\t}')
  cat(citation)
}
