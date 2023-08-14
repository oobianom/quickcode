#' Add elements to a vector like array_push in PHP
#'
#' Shorthand to add elements to a vector and save as the same name
#'
#' @param . first vector
#' @param add vector to add
#' @return vector combining fist and second vector, but have name set to the first
#' @examples
#' num1 <- sample(330:400,10)
#' num1 #preview
#' num2 <-"rpkg.net"
#'
#'
#' #seamlessly add num2 to num1 and re-save as num1
#'
#' vector_push(num1,num2)
#' num1 #updated with num2
#' num2 #not updated
#'
#' @export
#'
vector_push <- function(., add) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  assign(as.character(..), c(get(as.character(..), envir = parent.frame()), add), envir = parent.frame())
}

