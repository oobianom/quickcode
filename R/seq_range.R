#' Increment a number till a length is reached
#'
#' Designed to increment a number by a specified value
#' until a predefined count is reached.
#'
#' @param start NUMERIC. starting count
#' @param increment NUMERIC. number to increment by
#' @param count NUMERIC. length of output
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
#' seq3(1,6) #add 6 until 10 numbers are generated
#' seq3(1,2,7) # add 2 until 7 numbers are generated
#' seq3(133,7,6)# add 7 until 6 numbers are generated
#' seq3(1111,40,100)# add 40 until 100 numbers are generated
#'
#' @export
#'
seq3 <- function(start,increment,count = 10) cumsum(c(start,rep(increment,count-1)))



#' Check whether a number falls within a specified range of values and provide closes values
#'
#'
#' @description With a defined range of values, the function systematically examines
#' each provided number to determine if it falls within the specified range. It may
#' also provide the values with the range that are closest to a desired number.
#'
#' @param value NUMERIC. the vector of numbers to check
#' @param range.min NUMERIC. OPTIONAL. the minimum value of the range
#' @param range.max NUMERIC. OPTIONAL. the maximum value of the range
#' @param range.vec NUMERIC. OPTIONAL. a vector of numbers to use for the range
#' @param closest BOOLEAN. OPTIONAL. return closest value
#' @param rm.na BOOLEAN. OPTIONAL. remove NA values from input
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
#' # Task 5: remove NAs prior to processing
#' in.range(c(1,3,NA,3,4,NA,8), range.min = 4, range.max = 6, rm.na = FALSE) # do not remove NA
#' in.range(c(1,3,NA,3,4,NA,8), range.min = 4, range.max = 6, rm.na = TRUE) # remove NA
#' #in.range(c(NA), range.min = 4, range.max = 6, rm.na = TRUE) #This will return error
#'
#' # Task 6: return the closest number to the value
#' in.range(1:5, range.vec = 12:19, closest = TRUE)
#' in.range(c(1:5,NA,6:9), range.vec = 4:19, closest = TRUE)
#' in.range(c(1:5,NA,6:9), range.vec = 4:19, closest = TRUE, rm.na = TRUE)
#'
#' @export

in.range <- function(value, range.min, range.max, range.vec = NULL, closest = FALSE, rm.na = FALSE){
  #remove NA if set to TRUE
  if(rm.na) value <- value[not.na(value)]

  #if closest is true, then range.vec must not be null
  if(closest){
    if(is.null(range.vec)) stop("If 'closest' is TRUE, then 'range.vec' must not be NULL, see examples.")
    if(length(range.vec)<2) stop("If 'closest' is TRUE, then 'range.vec' must not have at least 2 values, see examples.")
  }

  #exit if value to check is not numeric
  stopifnot(all(is.numeric(value)))

  #if range.vec is set, use the value to set range.min and range.max
  if(not.null(range.vec) & length(range.vec) > 1){
  stopifnot(all(is.numeric(range.vec)))
    computRange <- range(range.vec)
    range.min <- computRange[1]
    range.max <- computRange[2]
  }
  #return range check
  if(closest){
    stats::setNames(as.numeric(apply(data.frame(value),1,function(x){
      names(sort(stats::setNames(abs(x - range.vec),range.vec))[1])
    })),value)
  }else{
    with(data.frame(less = range.min <= value, more = range.max >= value),less & more)
  }
}
