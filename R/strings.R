#' Generate a random string
#'
#' Create a random string of specified length
#'
#' @param n number of strings to create
#' @param length length of string to create
#' @return one more random string of specific length
#'
#' @examples
#' # Task 1: create 1 random string string of length 5
#' randString(n = 1, length = 5)
#'
#' # Task 2: create 5 random string string of length 10
#' randString(n = 5, length = 10)
#'
#'
#' # Task 3: create 4 random string string of length 16
#' randString(n = 4, length = 16)
#'
#' @export

randString <- function(n, length) {
  .combo <- c(0:9, letters, 0:9, LETTERS)
  .all <- matrix(sample(.combo, n * length, replace = TRUE), ncol = n)
  unlist(lapply(1:ncol(.all), function(i)paste(.all[, i], collapse = "")))
}
