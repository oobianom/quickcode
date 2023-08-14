#' Increment vector by value
#'
#' Increment the content of a vector and resave as the vector
#'
#' @param . vector of number(s)
#' @param add number to add
#' @return vector combining fist and second vector, but have name set to the first
#' @examples
#' num1 <- sample(330:400,10)
#' num1
#'
#' # increment num1 by 1
#' num1 #before increment
#' inc(num1)
#' num1 #after increment
#'
#' # increment num1 by 5
#' inc #before increment
#' inc(num1, add= 5)
#' inc #after increment
#' @export
#'
inc <- function(., add = 1) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  assign(as.character(..), (get(as.character(..), envir = parent.frame()) + add), envir = parent.frame())
}

#' @inherit inc
#' @export
plus <- inc


#' Decrease vector by value
#'
#' decrement the content of a vector and resave as the vector
#'
#' @param . vector of number(s)
#' @param add number to add
#' @return vector combining fist and second vector, but have name set to the first
#' @examples
#' num1 <- sample(330:400,10)
#' num1
#'
#' # decrement num1 by 1
#' num1 #before decrement
#' minus(num1)
#' num1 #after decrement
#'
#' # decrease num1 by 5
#' minus #before decrement
#' minus(num1, add= 5)
#' num1 #after decrement
#' @export
#'
minus <- function(., n = 1) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  assign(as.character(..), (get(as.character(..), envir = parent.frame()) - n), envir = parent.frame())
}
