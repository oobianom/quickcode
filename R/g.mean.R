#' Calculate geometric mean and round
#'
#' Calculate the geometric mean
#'
#' @param num vector of numbers
#' @param na.rm remove NAs from the vector
#' @param neg.rm remove negative values from the vector
#' @param round round result to decimal place
#' @return the geometric mean of a set of numbers
#' @examples
#' num1 <- sample(300:3000,10)
#'
#' #get the geometric mean, excluding all negatives and round to 2
#' geo.mean(num1)
#'
#' #or
#' geo.mean(num1)
#'
#'
#' #get geometric mean, but round the final value to 5 decimal places
#' geo.mean(num1, round = 5)
#'
#' @export

geo.mean <- function(num, na.rm = TRUE, neg.rm = TRUE, round = 2) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  if(neg.rm) num <- num[num > 0]
  return(round(exp(base::sum(log(num), na.rm = na.rm) / length(num)),round))
}
