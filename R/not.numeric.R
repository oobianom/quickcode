#' Not numeric
#'
#' Check if entry is not numeric
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is numeric
#' @examples
#' not.numeric("45") # TRUE
#' not.numeric(45) # FALSE
#' if(not.numeric(45)) message("yes") # yes
#'
#' @export

not.numeric <- function(x) !is.numeric(x)

