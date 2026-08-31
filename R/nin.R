#' Not in vector or array
#'
#' Check if entry is in vector
#'
#' @rdname nin
#' @param x vector entry
#' @param table table of items to check
#' @return a boolean value to indicate if entry is present
#' @examples
#' 5 %nin% c(1:10) #FALSE
#' 5 %nin% c(11:20) #TRUE
#'
#' x = "a"
#' if(x %nin% letters) x
#'
#' # let's say we are trying to exclude numbers from a vector
#' vector_num1 <- number(9, max.digits = 5, seed = 1) #simulate 9 numbers
#' vector_num1 #values
#' vector_num1[vector_num1 %nin% c(83615,85229)]#return values not 83615 or 85229
#' @export

`%nin%` <- function(x, table)!{
  (x %in% table)
}

`%nin%` -> `%!in%`
