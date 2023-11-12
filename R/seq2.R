#' Increment a number till a length is reached
#'
#' Designed to increment a number by a specified value
#' until a predefined count is reached.
#'
#' @param start starting count
#' @param increment number to increment by
#' @param count length of output
#'
#' @return numbers dictated by increment number
#'
#' @details
#' Users can easily leverage this function to automate the incrementation process,
#' making it a convenient solution for repetitive tasks. It is just a surrogate to the
#' seq() function, more useful depending on the goal of the user.
#'
#' @examples
#' # task 1: starting with 2, increment by 10 until 5 numbers are obtained
#' seq3(2,10,count=5)
#'
#' # task 2: starting with 4, increment by 4 until 4 numbers are archieved
#' # this is similar (but simpler) to obtaining 4 multiples of 4
#' # this may also be achieved with seq function like
#' # seq(4,16,4) or seq(4,16,length.out=4)
#' seq3(4,4,4)
#'
#' # other examples
#' seq3(1,6)
#' seq3(1,6,7)
#' seq3(133,7,6)
#' seq3(1111,40,100)
#'
#' @export
#'
seq3 <- function(start,increment,count = 10) cumsum(c(start,rep(increment,count-1)))


