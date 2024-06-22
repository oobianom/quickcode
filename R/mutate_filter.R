#' Mutate a subset of dataset, and reattach it to the dataset
#'
#' Extension of the dplyr::mutate function that allows the user to mutate only a specific filtered subset of a data, while leaving the other parts of the data intact
#' @return data frame containing original data, but with a subset mutated
#' @param subset subset of data to modify
#' @param ... mutation syntax similar to dplyr::mutate
#'
#' @examples
#' #mutate a subsection filter of mtcars
#' dt =mtcars
#' mutate_filter(dt,mpg == 21.0 & cyl == 6, cyl=1000,hp=2000,vs=hp*2)
#' @export
#'

mutate_filter <- function(., subset, ...) {
  .dt <- .
  #.dt$refonumval__92 <- 0
  .dt$refonumval__91 <- 1:nrow(.dt)
  .half1 <- .dt
  .fi <- as.character(list(substitute(subset)))
  eval(parse(text = paste0(".half2 = subset(.half1,", .fi, ");.half1 = subset(.half1,!(", .fi, "))")))
  .half2 <- transform(.half2,...)
  .full <- rbind(.half1, .half2[,names(.half1)])
  .full[order(.full$refonumval__91), names(.full) %nin% c("refonumval__91", "refonumval__92")]
}

#dt = mtcars
#mutate_filter(dt,mpg == 21.0 & cyl == 6, cyl=1000,hp=2000,vs=hp*2)


