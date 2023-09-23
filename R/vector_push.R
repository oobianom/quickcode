#' Add elements to a vector like array_push in PHP
#'
#' Shorthand to add elements to a vector and save as the same name
#'
#' @param . first vector
#' @param add vector to add
#' @param unique remove duplicated entries
#' @return vector combining fist and second vector, but have name set to the first
#' @examples
#' num1 <- number(10)
#' num2 <-"rpkg.net"
#'
#' num1
#' num2
#'
#' #Task: add num2 to num1 and re-save as num1
#' vector_push(num1,num2)
#' num1 #updated with num2
#' num2 #not updated
#'
#'
#' #Task: concatenate two vectors and remove duplicates
#' vector1 = 1:10
#' vector2 = 5:20
#' vector3 = 15:25
#'
#' # with duplicated
#' vector_push(vector1,vector2, unique = FALSE)
#' vector1 #return modified vector
#'
#' # without duplicated
#' vector_push(vector2,vector3, unique = TRUE)
#' vector2 #return modified vector
#' @export
#'
vector_push <- function(., add, unique = FALSE) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  .res <- c(get(as.character(..), envir = parent.frame()), add)
  if(unique) .res <- unique(.res)
  assign(as.character(..), .res , envir = parent.frame())
}

