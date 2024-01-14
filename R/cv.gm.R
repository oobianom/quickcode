
#' Calculate geometric coefficient of variation and round
#'
#' Calculate the coefficient of variation and round
#' @rdname geostats
#' @param num vector of numbers
#' @param na.rm remove NAs from the vector
#' @param neg.rm remove negative values from the vector
#' @param pct TRUE or FALSE. should result be in percent
#' @param round round result to decimal place
#' @return the geometric cv of a set of numbers
#' @examples
#' #simulate numbers using a fixed seed
#' num1 <- number(n = 1115,max.digits = 4, seed = 10)
#'
#' #get geometric CV, represent as percent and round to 2 decimal places
#' geo.cv(num1,round = 2) # result: 60.61%
#'
#' #or round to 3 decimal places
#' geo.cv(num1,round = 3) # result: 60.609%
#'
#' #by default, the above examples return a CV%
#' #if you do not want the result as percentage, specify "pct"
#' geo.cv(num1,pct = FALSE) # result: 0.61
#'
#' @export

geo.cv <- function(num, round = 2, na.rm = TRUE, neg.rm = TRUE, pct = TRUE) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  if(neg.rm) num <- num[num > 0]
  res <- sqrt(exp(sd(log(num), na.rm = na.rm)^2) - 1)
  if (pct) res <- res * 100
  round(res,round)
}

