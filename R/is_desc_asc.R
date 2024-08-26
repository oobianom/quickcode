#' Check is numbers in a vector is decreasing or increasing
#'
#' @rdname descasc
#' @export

is.increasing <- function(.){
  . <- as.numeric(.)
  .. <- sort(., decreasing = FALSE)
  all(.. == .)
}


#' @rdname descasc
#' @export

is.decreasing <- function(.){
  . <- as.numeric(.)
  .. <- sort(., decreasing = TRUE)
  all(.. == .)
}
