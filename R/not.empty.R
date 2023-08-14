#' Not empty
#'
#' Check if entry is not empty
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is empty
#' @examples
#' not.empty("empty") # TRUE
#' not.empty('') # FALSE
#' not.empty(y<-NULL) # FALSE
#' if(not.empty('')) message("yes") # NULL
#' @export

not.empty <- function(x){
  eval <- not.null(x) & (x != '')
  if(!length(eval)) eval <- FALSE
  eval
}
