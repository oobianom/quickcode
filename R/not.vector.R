#' Not a vector
#'
#' Opposite of is.vector(). Check if entry is not vector
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is vector
#' @examples
#' vect1 = list(r=1,t=3:10)
#' vect2 = LETTERS
#' is.vector(vect1) # TRUE
#' not.vector(vect1) # FALSE
#' not.vector(vect2) # FALSE
#' if(not.vector(vect1)) message("yes") # NULL
#'
#' @export

not.vector <- function(x)!{
  is.vector(x)
}
