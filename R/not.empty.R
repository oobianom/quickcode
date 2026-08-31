#' Not empty
#'
#' Check if entry is not empty
#'
#' @rdname empty
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

#' @rdname empty
#' @export

is.empty <- function(x)!{
  not.empty(x)
}

#' Not exists
#'
#' Check if object does not exists
#'
#' @param x object
#' @return a boolean value to indicate if entry does not exists
#' @examples
#' go = 7
#' not.exists("exis") # TRUE
#' not.exists("go") # FALSE
#' if(not.exists('hallo')) message("yes") # NULL
#' @export

not.exists <- function(x){
  !exists(x)
}
