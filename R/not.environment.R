
#' Not an environment
#'
#' Check if entry is not an environment object
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is an environment
#' @examples
#' test.env <- new.env()
#' test.notenv <- list(t=1)
#' not.environment(test.env) # FALSE
#' not.environment(test.notenv) # TRUE
#' if(not.environment(test.notenv)) message("yes") # yes
#'
#' @export

not.environment <- function(x) !is.environment(x)
