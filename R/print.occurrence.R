#' @export
print.occurrence <- function(x, '...') {
  citation <- paste0("EarthLife Consortium. ",
                     format(x$meta$access, "%Y"), ". ",
                     x$meta$title, ". ",
                     "Access Date:",
                     format(x$meta$access, "%F"), "; ",
                     "URL: ",
                     x$meta$call, '\n\n')

  uniques <- nrow(x$records) - sum(duplicated(x$records[,c("lng", "lat")]))
  age_range <- c(range(x$records$lag, na.rm = TRUE)[1],
                 range(x$records$eag, na.rm = TRUE)[2])

  record <- paste0("Occurrence data from the EarthLife Consortium.\n",
                   nrow(x$records), " returned, ", uniques, " unique sites.\n",
                   length(unique(x$records$tna)), " identified taxa from ",
                   age_range[1], " to ", age_range[2], "Mya.")
}
