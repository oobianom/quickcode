
#' Calculate geometric coefficient of variation and round
#'
#' Calculate the coefficient of variation and round
#'
#' @param num vector of numbers
#' @param na.rm remove NAs from the vector
#' @param pct TRUE or FALSE. should result be in percent
#' @param round round result to decimal place
#' @return the geometric cv of a set of numbers
#' @examples
#' num1 <- sample(330:400,15)
#'
#' #get geometric CV, represent as percent and round to 3 decimal places
#' cv.gm(num1,round = 3)
#'
#' #or
#' geo.cv(num1,round = 3)
#'
#' @export

cv.gm <- function(num, na.rm = TRUE, pct = TRUE, round = 2) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  res <- sqrt(exp(sd(log(num[num > 0]), na.rm = na.rm)^2) - 1)
  if (pct) res <- res * 100
  round(res,round)
}


#' @inherit cv.gm
#' @export

cv.gm -> geo.cv
