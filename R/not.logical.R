#' Not logical
#'
#' Opposite of is.logical(). Check if entry is a logical object
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is logical
#' @examples
#' test.env <- TRUE
#' test.notenv <- 0
#' not.logical(test.env) # FALSE
#' not.logical(test.notenv) # TRUE
#' if(not.logical(test.notenv)) message("yes") # yes
#'
#' @export

not.logical <- function(x)!{
  is.logical(x)
}
