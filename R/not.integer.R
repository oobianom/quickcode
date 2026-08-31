#' Not an integer
#'
#' Opposite of is.integer(). Check if entry is not an integer
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is an integer
#' @examples
#' is.integer(78L) #TRUE
#' not.integer(78L) #FALSE
#'
#' not.integer(23.43) # TRUE
#' not.integer(45L) # FALSE
#' if(not.integer(4L)) message("yes") # NULL
#'
#' @export

not.integer <- function(x)!{
  is.integer(x)
}
