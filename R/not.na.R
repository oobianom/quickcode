#' Not NA
#'
#' Opposite of is.na(). Check if entry is not NA
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is NA
#' @examples
#' not.na(NA) # FALSE
#' not.na(NULL) # logical(0)
#' if(not.na(45)) message("something") # TRUE
#'
#' @export

not.na <- function(x)!{
  is.na(x)
}
