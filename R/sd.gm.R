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

