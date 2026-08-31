#' Generate a random number (integer)
#'
#' Fetch n random integers between 1 and 1,000,000,000
#'
#' @param n how many numbers to generate
#' @param max.digits maximum number of digits in each number
#' @param seed set seed for sampling to maintain reproducibility
#' @return random numbers between 1 and 1 billion
#'
#' @examples
#' number(1)
#' number(10)
#' paste0(number(2),LETTERS)
#'
#' #set maximum number of digits
#' number(1,max.digits = 5)
#' number(10,max.digits = 4)
#'
#' #set seed for reproducibility
#' #without seed
#' number(6) #result 1
#' number(6) #result 2, different from result 1
#' #with seed
#' number(6,seed=1)#result 3
#' number(6,seed=1)#result 4, same as result 3
#' @export
#'
number <- function(n,max.digits=10,seed=NULL){
  if(not.null(seed))set.seed(seed)
  as.integer(substr(sample(1L:1000000000L, n),0,max.digits))
}
