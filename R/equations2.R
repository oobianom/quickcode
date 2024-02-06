#' Calculate the distance of points from the center of a cluster
#'
#' This function operates on multivariate data and
#' calculates the distance of points from the centroid of one or more clusters.
#'
#' @rdname distance
#' @param data dataset with atleast 2 columns
#' @param n Number of observations (scaler)
#' @param v A vector numerically referencing which column to select;
#' variable references can be contiguous, non-contiguous,
#' or a combination as defined; E.g. v = c(3,6:9)
#' @param neg.rm remove negative values from the vector
#' @param round round result to decimal place
#' @return The function returns a named vector consisting of a row number and a pair-distance value
#'
#' @section Function utility:
#' Used to generate the computations needed to model pair-distance measures in three dimensions
#'
#' @examples
#'
#' v= c(2,4:10)
#' n = 6
#' data = attenu[,1:2]
#'
#' #basic example
#' pairDist(data)
#'
#' # specify number of observations
#' pairDist(data, n, v)
#'
#' # round results to 2 decimal points
#' pairDist(data, n, v,2)
#'
#' @export

pairDist <- function(data, n, v, round = NULL) {
  message("This function is still being developed")
  if(missing(n) | missing(v))
    res <- sqrt(rowSums((data- matrix(colMeans(data)))^2))
  else
  res <- sqrt(rowSums((data- matrix(colMeans(data), n, v, byrow = TRUE))^2))
  if(typeof(round) == "double") res <- round(res,round)
  res
}




# Other equations - Still need to build a story around each equation

#' #' Production Function (Cobb-Douglas)
#' #'
#' #' Calculate the output
#' #'
#' #' @rdname economics
#' #' @param Q output
#' #' @param L labour output
#' #' @param K caputal input
#' #' @param A total factor
#' #' @param alpha output elacticity
#' #' @param beta output elasticity
#' #'
#' #'
#' #'
#' cobbDouglas <- function(Q, A, L, K, alpha, beta) {
#'   Q <- A * (L^alpha) * (K^beta)
#' }
#'
#'
#' #' Phillips Curve
#' #'
#' #'
#'
#' infrate <- function(eInf, a, u, u0, round = 2) {
#'   round(eInf - a(u - u0), round)
#' }
#'
#'
#' #' Laffer Curve
#' #'
#' #' Calculate tax revenue
#' #'
#' #' @param T tax revenue
#' #' @param R tax rate
#' #' @param GDP gross domestic product
#' #'
#' #'
#'
#' lafferCurve <- function(T, R, GDP, round = 2) {
#'   x <- NA
#'   if (missing(T)) {
#'     x <- (R * GDP) / 2
#'   } else if (missing(Km)) x <- T * 2 / GDP
#'   round(x, round)
#' }
#'
#' #' Michaelis-Menten Equation
#' #'
#' #'
#'
#' MMk <- function(V, Vmax, S, Km, round = 2) {
#'   x <- NA
#'   if (missing(V)) {
#'     x <- Vmax * S / (Km + S)
#'   } else if (missing(Km)) x <- (Vmax * S / V) - S
#'   round(x, round)
#' }
#'
#' #' Henderson-Hasselbalch Equation
#' #'
#' #'
#' #'
#'
#' pHpKa <- function(pH, pKa, A, HA, round = 2) {
#'   x <- NA
#'   if (missing(pH)) {
#'     x <- pKa + log(A / HA)
#'   } else if (missing(pKa)) x <- pH - log(A / HA)
#'   round(x, round)
#' }
#'
#'
#'
#'
#' #' Lineweaver-Burk Equation (Double reciprocal plot)
#' #'
#' #'
#' #'
#'
#' linweaver <- function(V,Km,Vmax,S, round = 2) {
#'   x <- NA
#'   if (missing(V)) {
#'     x <- Vmax * s / (Km + S)
#'   } else if (missing(Km)) x <- (Vmax * S / (V)) - S
#'   round(x, round)
#' }
