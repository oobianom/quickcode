#' Add elements to a list like array_push in PHP
#'
#' Shorthand to add elements to a vector and save as the same name
#'
#' @param . first list
#' @param add list to add
#' @return vector combining fist and second vector, but have name set to the first
#' @examples
#' num1 <- list(sample(330:400,10))
#' num2 <-list("rpkg.net")
#' list_push(num1, add= num2)
#' @export
#'
list_push <- function(., add) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  assign(as.character(..), list(get(as.character(..), envir = parent.frame()), add), envir = parent.frame())
}
