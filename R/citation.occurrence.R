#' @export
citation.occurrence <- function(x, '...') {

  citation <- paste0("@misc{ELC_", format(x$meta$access, "%F"), ',\n',
                     "\tauthor={EarthLife Consortium},\n",
                     "\ttitle={",    x$meta$title, "},\n",
                     "\tyear={",     format(x$meta$access, "%Y"), "},\n",
                     "\turldate={",  format(x$meta$access, "%F"), "},\n",
                     "\turl={",x$meta$call, '}\n}')
  cat(citation)
}
