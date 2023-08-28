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
#' g.mean(num1)
#'
#' @export

g.mean <- function(num, na.rm = TRUE, neg.rm = TRUE, round = 2) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  if(neg.rm) vector_push(num[num > 0])
  return(round(exp(base::sum(log(num), na.rm = na.rm) / length(num)),round))
}


#' @inherit g.mean
#' @export

g.mean -> geo.mean
