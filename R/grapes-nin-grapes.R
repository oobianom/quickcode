#' Not in vector or array
#'
#' Check if entry is in vector
#'
#' @param x vector entry
#' @param table table of items to check
#' @return a boolean value to indicate if entry is present
#' @examples
#' 5 %nin% c(1:10) #FALSE
#' 5 %nin% c(11:20) #TRUE
#'
#' x = "a"
#' if(x %nin% letters) x
#' @export

`%nin%` <- function(x, table) {
  !(x %in% table)
}
`%nin%` -> `%!in%`
