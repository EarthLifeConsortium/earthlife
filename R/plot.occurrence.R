#' @export
#'
plot.occurrence <- function(x, ...) {
  par(mfrow = c(1, 2))

  unique_sites <- !duplicated(x$records[, c("lat", "lng")])

  plot(density(log(x$records$max_age), na.rm = TRUE),
       main = "")

  map('world', fill = FALSE,
      col = "darkgray",
      xlim = range(x$records$lng),
      ylim = range(x$records$lat))

  points(x$records[unique_sites, c("lng", "lat")],
       main = "Site Locations",
       ylab = "Latitude",
       xlab = "Longitude",
       pch = 19, cex = 0.5,
       col = rgb(0, 0, 0, 0.1))

}
