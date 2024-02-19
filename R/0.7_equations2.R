#' Calculate the distance of points from the center of a cluster
#'
#' This function operates on multivariate data and
#' calculates the distance of points from the centroid of one or more clusters.
#'
#' @rdname distance
#' @param data dataset with at least 2 columns
#' @param n Number of observations (scaler)
#' @param round round result to decimal place
#' @return a named vector consisting of a row number and a pair-distance value
#'
#' @section Function utility:
#' Used to generate the computations needed to model pair-distance measures in three dimensions
#'
#' @references the current function was adapted from one of the examples in the svgViewR package, \cr
#' https://cran.r-project.org/web/packages/svgViewR/svgViewR.pdf
#' @examples
#' n = 6
#' data = attenu[,1:2]
#'
#' #basic example
#' pairDist(data)
#'
#' # specify number of observations
#' pairDist(data, n)
#'
#' # round results to 2 decimal points
#' pairDist(data, n,2)
#'
#' @export

pairDist <- function(data, n, round = NULL) {

  # check entry arguments
  if(length(data) %% n > 0) stop("n must be a multiple of data length")

  # check dataset to make sure it had at least 2 columns
  if(typeof(data) == "list")stopifnot(ncol(data) >= 2)

  # compute based on what is provided
  if (missing(n)) {
    res <- sqrt(rowSums((data - matrix(colMeans(data)))^2))
  } else {
    # validate value of n
    stopifnot(n >= 1, n <= nrow(data))
    res <- sqrt(rowSums((data - matrix(colMeans(data), n, byrow = TRUE))^2))
  }

  # round result if specified
  if (not.null(round)) res <- round(res, round)

  # return
  res
}




