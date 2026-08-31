#' Not NULL
#'
#' Opposite of is.null(). Check if entry is not NULL
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is NULL
#' @examples
#' is.null("") # FALSE
#' not.null("") # TRUE
#' not.null(NULL) # FALSE
#' if(not.null(45)) message("something") # yes
#'
#' @export

not.null <- function(x)!{
  is.null(x)
}
