#' Mutate a subset of dataset, and reattach it to the dataset
#' @param filter subset of data to modify
#' @param ... mutation syntax similar to dplyr::mutate
#' @export
#'

mutate_filter <- function(., filter., ...) {
  .dt <- .
  .dt$refonumval__91 <- 1:nrow(.dt)
  .half1 <- .dt
  .fi <- as.character(list(substitute(filter.)))
  eval(parse(text = paste0(".half2 = subset(.half1,", .fi, ");.half1 = subset(.half1,!(", .fi, "))")))
  .half2 <- dplyr::mutate(.half2, ...)
  .full <- rbind(.half1, .half2)
  .full[order(.full$refonumval__91), names(.full) %nin% c("refonumval__91")]
}

