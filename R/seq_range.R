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



#' Verify whether a number falls within a specified range of values.
#'
#'
#' @description With a defined range of values, the function systematically examines
#' each provided number to determine if it falls within the specified range.
#'
#' @param value the vector of numbers to check
#' @param range.min the minimum value of the range
#' @param range.max the maximum value of the range
#' @param range.vec a vector of numbers to use for the range
#'
#' @return boolean to indicate if the value or set of values are within the range
#' @note
#' The argument range.vec is utilized when users opt not to employ the range.min or
#' range.max arguments. If range.vec is specified, range.min and range.max are
#' disregarded. It's important to note that the use of range.vec is optional.
#'
#' @details
#' The described function serves the purpose of checking whether a given number
#' or set of numbers falls within a specified range. It operates by taking a range
#' of values as input and then systematically evaluates each provided number to
#' determine if it lies within the defined range. This function proves particularly
#' useful for scenarios where there is a need to assess numeric values
#' against predefined boundaries, ensuring they meet specific criteria or constraints.
#'
#' @examples
#' # Task 1: Check if a number is within specified range
#' in.range(5, range.min = 3, range.max = 10) # TRUE
#' in.range(25, range.min = 12, range.max = 20) # FALSE
#'
#' # Task 2: Check if a set of values are within a specified range
#' in.range(1:5, range.min = 2, range.max = 7) #
#' in.range(50:60, range.min = 16, range.max = 27) #
#'
#'
#' # Task 3: Check if a number is within the range of a set of numbers
#' in.range(5, range.vec = 1:10) # TRUE
#' in.range(345, range.vec = c(1001,1002,1003,1004,1005,
#' 1006,1007,1008,1009,1010,1011,1012,1013,1014)) # FALSE
#'
#' # Task 4: Check if a set of values are within the range of a set of numbers
#' in.range(1:5, range.vec = 4:19) #
#' in.range(50:60, range.vec = c(55,33,22,56,75,213,120)) #
#'
#'
#' @export

in.range <- function(value, range.min, range.max, range.vec = NULL){
  if(not.null(range.vec) & length(range.vec) > 1){
    computRange <- range(range.vec)
    range.min <- computRange[1]
    range.max <- computRange[2]
  }
  with(data.frame(less = range.min <= value, more = range.max >= value),less | more )
}
