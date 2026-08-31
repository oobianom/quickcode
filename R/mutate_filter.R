#' Mutate only a subset of dataset intact
#'
#' Extension of the dplyr::mutate function that allows the user to mutate only a specific filtered subset of a data, while leaving the other parts of the data intact
#' @return data frame containing original data, but with a subset mutated
#' @param . data object
#' @param sub.set subset of data to modify
#' @param ... mutation syntax similar to dplyr::mutate
#'
#' @examples
#' #mutate a subsection filter of mtcars
#' dt = mtcars
#' names(dt)
#' head(dt)
#' mutate_filter(dt,mpg == 21.0 & cyl == 6, cyl=1000,hp=2000,vs=hp*2)
#'
#'
#' dt2 = beaver1
#' names(dt2)
#' head(dt2)
#' mutate_filter(dt2, day == 346 & time < 1200, activ = 12, temp = round(temp*10,1))
#' @export
#'

mutate_filter <- function(., sub.set, ...) {
  stopifnot(inherits(.,"data.frame"))
  .half1 <- .
  .half1$refonumval__91 <- 1:nrow(.half1)
  .fi <- as.character(list(substitute(sub.set)))
  exectext <- paste0(".half2 = subset(.half1,", .fi, ");.half1 = subset(.half1,!(", .fi, "))")
  eval(parse(text = exectext))
  .half2 <- transform(.half2,...)
  .full <- rbind(.half1, .half2[,names(.half1)])
  .full[order(.full$refonumval__91), names(.full) %nin% "refonumval__91"]
}



