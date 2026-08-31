#' Check is numbers in a vector are decreasing or increasing
#'
#' Test if numeric values of a vector are decreasing or increasing using the radix method
#'
#' @param . a numeric vector
#' @param na.last for controlling the treatment of NAs. If TRUE, missing values in the data are put last; if FALSE, they are put first; if NA, they are removed.
#' @rdname descasc
#' @return boolean value to indicate if the values are increasing or decreasing
#'
#' @examples
#' # example code
#' doy1 <- rnorm(1e3)
#' doy2 <- sort(doy1, decreasing = FALSE)
#' doy3 <- sort(doy1, decreasing = TRUE)
#'
#' is.increasing(doy1)
#' is.decreasing(doy1)
#'
#' is.increasing(doy2)
#' is.decreasing(doy2)
#'
#' is.increasing(doy3)
#' is.decreasing(doy3)
#' @export

is.increasing  <- function(., na.last = TRUE)!{
  is.unsorted(sort(as.numeric(.), decreasing = FALSE, na.last = na.last, method = "radix", index.return = TRUE)$ix)
}


#' @rdname descasc
#' @export

is.decreasing <- function(., na.last = TRUE){
  . <- as.numeric(.)
  .. <- sort(., decreasing = TRUE, na.last = na.last, method = "radix")
  all(.. == .)
}
#is.unsorted(rev(sort(as.numeric(.), decreasing = FALSE, na.last = na.last, method = "radix", index.return = TRUE)$ix))


