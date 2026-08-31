#' Not duplicated elements
#'
#' Opoosite of duplicated(). Checks which elements of a vector or data frame are NOT duplicates of elements with smaller subscripts
#'
#' @param x a vector or a data frame or an array or NULL.
#' @param incomparables a vector of values that cannot be compared. FALSE is a special value, meaning that all values can be compared, and may be the only value accepted for methods other than the default. It will be coerced internally to the same type as x
#' @param ... arguments for particular methods.
#' @return elements of a vector or data frame that are NOT duplicates
#' @examples
#'
#' set.seed(08082023)
#' dtf <- sample(1:10,15, replace = TRUE)
#' dtf # 3  9 10  3  8  9  6 10  5  1  2  2  2  9  8
#' dtf[ dtf > 4 & duplicated(dtf) ] # 9 10  9  8
#' dtf[ dtf > 4 & not.duplicated(dtf) ] # 9 10  8  6  5
#'
#' @export

not.duplicated <- function(x, incomparables = FALSE, ...)!{
  duplicated(x = x, incomparables = incomparables, ...)
}


