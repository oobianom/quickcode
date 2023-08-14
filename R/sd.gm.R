#' Calculate geometric standard deviation and round
#'
#' Calculate the geometric standard deviation
#'
#' @param num vector of numbers
#' @param na.rm remove NAs from the vector
#' @param round round result to decimal place
#' @return the geometric standard deviation of a set of numbers
#' @examples
#' num1 <- sample(330:400,10)
#' sd.gm(num1,na.rm=FALSE)
#'
#' @export

sd.gm <- function(num, na.rm = TRUE, round = 2) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  return(round(exp(stats::sd(log(num[num > 0]), na.rm = na.rm)),round))
}
