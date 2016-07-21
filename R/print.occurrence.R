#' @export
print.occurrence <- function(x, ...) {
  citation <- paste0("EarthLife Consortium. ",
                     format(x$meta$access, "%Y"), ". ",
                     x$meta$title, ". ",
                     "Access Date:",
                     format(x$meta$access, "%F"), "; ",
                     "URL: ",
                     x$meta$call, '\n\n')

  uniques <- nrow(x$records) - sum(duplicated(x$records[,c("lng", "lat")]))
  age_range <- c(range(x$records$max_age, na.rm = TRUE)[1],
                 range(x$records$min_age, na.rm = TRUE)[2])

  record <- paste0("Occurrence data from the EarthLife Consortium.\n",
                   nrow(x$records), " returned, ", uniques, " unique sites.\n",
                   length(unique(x$records$accepted_name)), " identified taxa from ",
                   age_range[1], " to ", age_range[2], " Mya.\n")

  cat(record, " - ",format(x$meta$access, "%F"),"\n")
  print(x$record[1:3,c("lng", "lat", "accepted_name", "min_age", "max_age", "age_unit")])
  cat("\t...\n\n")
  cat("Use \'citation\' to get a formal citation for this search result.\n")

}
