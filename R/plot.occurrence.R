#' @importFrom maps map
#' @export
#'
plot.occurrence <- function(x, by_db = TRUE, ...) {

  # Set up a 1x2 plotting window (should set to default plot settings)
  old_par <- par()

  # if `by_db` is true then we split the densities up so that the difference
  # in sampling intensity is clearer:
  if (!by_db) {
    par(mfrow = c(1, 2))
  }


  # Clear duplicate sites)
  unique_sites <- !duplicated(x$records[, c("lat", "lng")])

  plot(density(log(x$records$max_age), na.rm = TRUE),
       main = "")

  maps::map('world', fill = FALSE,
      col = "darkgray",
      xlim = range(x$records$lng),
      ylim = range(x$records$lat))

  points(x$records[unique_sites, c("lng", "lat")],
       main = "Site Locations",
       ylab = "Latitude",
       xlab = "Longitude",
       pch = 19, cex = 0.5,
       col = rgb(0, 0, 0, 0.1))

  # Reset the plot window:
  par(old_par)

}
