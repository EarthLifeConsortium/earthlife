#' @export
print.occurrence <- function(x, ...) {

  uniques <- nrow(x$records) - sum(duplicated(x$records[,c("lng", "lat")]))
  age_range <- c(range(x$records$max_age, na.rm = TRUE)[1],
                 range(x$records$min_age, na.rm = TRUE)[2])

  access_date <- ifelse("access" %in% names(x$meta),
                        format(x$meta$access, "%F"),
                        paste0(sapply(x$meta, function(x) format(x$access, "%F")), collapse = ", "))

  record <- paste0("Occurrence data from the EarthLife Consortium.\n",
                   nrow(x$records), " occurrences returned from ", uniques, " unique sites.\n",
                   length(unique(x$records$accepted_name)), " identified taxa from ",
                   age_range[1], " to ", age_range[2], " Mya.\n",
                   "Accessed: ",access_date,"\n\n")

  cat(record)
  print(x$record[1:3,c("lng", "lat", "accepted_name", "min_age", "max_age", "age_unit")])
  cat("\t...\n\n")
  cat("Use \'citation\' to get a formal citation for this search result.\n")

}
