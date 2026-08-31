
#' Calculate geometric coefficient of variation, mean, or SD and round
#'
#' Calculate the coefficient of variation
#'
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



#' Calculate geometric mean and round
#'
#' Calculate the geometric mean
#'
#' @rdname geostats
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

geo.mean <- function(num, round = 2, na.rm = TRUE, neg.rm = TRUE) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  if(neg.rm) num <- num[num > 0]
  return(round(exp(base::sum(log(num), na.rm = na.rm) / length(num)),round))
}


#' Calculate geometric standard deviation and round
#'
#' Calculate the geometric standard deviation
#'
#' @rdname geostats
#' @param num vector of numbers
#' @param na.rm remove NAs from the vector
#' @param neg.rm remove negative values from the vector
#' @param round round result to decimal place
#' @return the geometric standard deviation of a set of numbers
#' @examples
#' num1 <- sample(330:400,20)
#'
#' #get geometric SD remove negative values and round to 2 decimal places
#' geo.sd(num1)
#'
#' #get geometric SD, DON'T remove negative values and round to 2 decimal places
#' geo.sd(num1,na.rm=FALSE)
#'
#' #get geometric SD, remove negative values and round to 3 decimal places
#' geo.sd(num1,round = 3)
#'
#' @export

geo.sd <- function(num, round = 2, na.rm = TRUE, neg.rm = TRUE) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  if(neg.rm) num <- num[num > 0]
  return(round(exp(stats::sd(log(num), na.rm = na.rm)),round))
}




