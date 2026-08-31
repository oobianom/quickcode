#' Miscellaneous math computations: Corresponding m-m and quantile for confident intervals
#'
#' @description
#'  Compute the corresponding quantile given confident interval bounds
#'
#' @rdname mathmisc
#' @param ci confident interval eg. 0.9 for 90 percent confident intervals
#' @param Vmax The maximum velocity of the enzymatic reaction.
#' @param S The substrate concentration.
#' @param Km The substrate concentration at which the reaction rate is half of Vmax.
#' @param V The current velocity of the enzymatic reaction
#' @param round round result to number of decimal places
#' @return vector of two numeric values for the quantile based on the confident interval chosen
#' @examples
#'
#' # Get the bounds for 90% confident intervals
#' math.qt(0.9)
#'
#' # Get the bounds for 95% confident intervals
#' # use the bounds to obtain quartile
#' values = number(100)
#' values
#' ci = math.qt(0.95)
#' getquart = quantile(values, probs = ci)
#' getquart
#'
#' @export


math.qt <- function(ci = 0.9){
  c(1 - (1-ci)/2,(1-ci)/2)
}
#' @return result of calculation of Michaelis-Menten equation
#' @export
#' @examples
#' math.mm(3,500,0.5)
#'
#' @rdname mathmisc
math.mm <- function(Vmax, S, Km, V, round = NULL) {
  x <- NA
  if (missing(V)) {
    x <- Vmax * S / (Km + S)
  } else if (missing(Km)) x <- (Vmax * S / V) - S
  if(!is.null(round)) x = round(x, round)
  x
}
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
