#' Not a data
#'
#' Opposite of is.data.frame(). Check if entry is not a data object
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is a data table
#' @examples
#' test.dt <- data.frame(ID=1:200,Type="RPKG.net")
#' test.notenv <- list(t=1)
#'
#' is.data.frame(test.dt) # TRUE
#' not.data(test.dt) # FALSE
#'
#' not.data(test.notenv) # TRUE
#' if(not.data(test.dt)) message("yes") # NULL
#'
#' @export

not.data <- function(x)!{
  is.data.frame(x)
}
