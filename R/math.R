# data_mut_mqt <- function(data, ci = 0.95) {
#   n <- length(data)
#
#   # Calculate quartiles
#   q1 <- quantile(data, probs = 0.25)
#   q2 <- median(data)
#   q3 <- quantile(data, probs = 0.75)
#
# }
#
#
# data_mut_mss <- function(., col, type = c("arit","geo")){
#   fild = .[,as.character(substitute(col))]
#   .[,'mean'] = mean(fild)
#   # within(.,{
#   #   mean = mean(col)
#   #   sd = sd(get(fild))
#   #   sem = sd/length(get(fild))
#   # })
#   .
# }

#' Corresponding quantile for confident intervals
#'
#' Compute the corresponding quantile given confident interval bounds
#'
#' @param ci confident interval eg. 0.9 for 90% confident intervals
#'
#' @return a vector of two numeric values for the quantile based on the confident interval chosen
#'
#' @examples
#'
#' # Get the bounds for 90% confident intervals
#' math.qt(0.9)
#'
#' # Get the bounds for 95% confident intervals
#' # use the bounds to obtain quartile
#' values = number(100)
#' ci = math.qt(0.95)
#' getquart = quantile(values, probs = ci)
#' print(getquart)
#'
#'
#'
math.qt <- function(ci = 0.9){
  c(1 - (1-ci)/2,(1-ci)/2)
}
