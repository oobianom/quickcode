#' Multiple a vector of numeric values
#'
#' Mulitple all the content of a vector
#'
#' @param ... the numeric values to multiply
#' @return multiple of all content
#'
#' @examples
#' # multiply 1 number
#' # returns error
#' # multiply(0)
#'
#' # vector of numbers
#' numvec <- number(10, max.digits = 3)
#' numvec
#'
#' # multiply 2 numbers
#' multiply(numvec[1:2])
#' multiply(numvec[4], numvec[5])
#' multiply(a = 4, b = 5)
#'
#' # multiply 5 numbers
#' multiply(numvec[1:5])
#' multiply(11, 15, 12, 14, 13)
#' multiply(a = 4, b = 22, c = 44, d = 9, u = 10)
#'
#' @export

multiply <- function(...) {
  args <- unlist(c(as.list(environment()), list(...)))
  stopifnot(length(args) > 1)
  utils::tail(cumprod(args), n = 1)
}


