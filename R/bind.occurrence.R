#' @export
bind.occurrence <- function(x, ...) {
  inputs <- list(x, ...)

  if (!all(sapply(inputs, function(x)"occurrence" %in% class(x)))) {
    stop("All objects must be 'occurrence' objects.")
  }

  occ_output <- list(records = do.call(rbind.data.frame, lapply(inputs, '[[', "records")),
                     meta    = lapply(inputs, '[[', "meta"))

  occ_output$records$call <- rep(paste0("ELC_", 1:length(inputs)), sapply(inputs, function(x)nrow(x$records)))

  if (any(duplicated(occ_output$records[,!colnames(occ_output$records) %in% "call"]))) {
    occ_output$records <- occ_output$records[!duplicated(occ_output$records[,!colnames(occ_output$records) %in% "call"]),]
    warning("There were duplicate records in the new 'occurrence' object.")
  }

  names(occ_output$meta) <- paste0("ELC_", 1:length(inputs))

  if (any(!names(occ_output$meta) %in% unique(occ_output$records$call))) {
    occ_output$meta <- occ_output$meta[-which(!names(occ_output$meta) %in% unique(occ_output$records$call))]
  }

  class(occ_output) <- c("occurrence", "list")

  return(occ_output)
}
