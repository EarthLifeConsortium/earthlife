#' @importFrom rworldmap getMap
#' @export
#'
plot.occurrence <- function(x, by_db = TRUE, round = 3, ...) {

  # Set up a 1x2 plotting window (should set to default plot settings)
  old_par <- par()

  # if `by_db` is true then we split the densities up so that the difference
  # in sampling intensity is clearer:
  if (!by_db) {
    par(mfrow = c(2, 2))
  } else {
    par(mfrow = c(1, 2))
  }

  ages <- na.omit(x$records[,c("min_age", "max_age", "database")])
  ages$min_age <- ages$min_age + 0.001
  ages[,1:2] <- round(ages[, 1:2], 3)

  ages <- ages[!duplicated(ages), ]

  plot_order <- order(ages$min_age, ages$max_age, na.last = TRUE)

  # Clear duplicate sites)
  unique_sites <- !duplicated(x$records[, c("lat", "lng")])

  world <- rworldmap::getMap(resolution = 'low')

  log_ages <- na.omit(suppressWarnings(log(x$records$max_age * 10e6)))
  log_ages <- log_ages[!is.infinite(log_ages)]

  ages <- ages[plot_order, ]

  plot(1,1, type = 'n',
       xlim = c(0.001, max(ages$max_age, na.rm = TRUE)),
       ylim = c(0, nrow(ages)),
       log = "x",
       xlab = "Millions of Years BP",
       ylab = "",
       yaxt = "n",
       xaxt = "n")

  colors <- c(rgb(0.1, 0.1, 0.1, 0.1, 0.5),
              rgb(1, 0, 0, 0, 0.5))

  for (i in 1:nrow(ages)) {
    segments(ages$min_age[i], i,
             ages$max_age[i], i,
             col = colors[as.numeric(factor(ages$database))[i]])
  }

  axis(1, at = (10 ^ pretty(log(ages$max_age * 10 ^ 6, 10))) / 10 ^ 6,
       labels = format((10 ^ pretty(log(ages$max_age * 10 ^ 6, 10))) / 10 ^ 6,
                       scientific = FALSE,
                       drop0trailing = TRUE))

  sp::spplot(world,
         1,
      fill = "darkgray",
      xlim = range(x$records$lng),
      ylim = range(x$records$lat),
      colorkey = FALSE)

  points(x$records[unique_sites, c("lng", "lat")],
       main = "Site Locations",
       ylab = "Latitude",
       xlab = "Longitude",
       pch = 19, cex = 0.5,
       col = rgb(0, 0, 0, 0.1))

  # Reset the plot window:
  par(old_par)

}
