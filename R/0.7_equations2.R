#' Calculate the distance of points from the center of a cluster
#'
#' This function operates on multivariate data and
#' calculates the distance of points from the centroid of one or more clusters.
#'
#' @rdname distance
#' @param data dataset with at least 2 columns
#' @param n Number of observations (scaler)
#' @param v A vector numerically referencing which column to select;
#' variable references can be contiguous, non-contiguous,
#' or a combination as defined; E.g. v = c(3,6:9)
#' @param round round result to decimal place
#' @return a named vector consisting of a row number and a pair-distance value
#'
#' @section Function utility:
#' Used to generate the computations needed to model pair-distance measures in three dimensions
#'
#' @references the current function was adapted from one of the examples in the svgViewR package, https://cran.r-project.org/web/packages/svgViewR/svgViewR.pdf
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

  # check dataset to make sure it had at least 2 columns
  stopifnot(typeof(data) == "list", ncol(data) >= 2)

  # compute based on what is provided
  if (missing(n) | missing(v)) {
    res <- sqrt(rowSums((data - matrix(colMeans(data)))^2))
  } else {
    # validate value of n
    stopifnot(n >= 1, n <= nrow(data))
    res <- sqrt(rowSums((data - matrix(colMeans(data), n, v, byrow = TRUE))^2))
  }

  # round result if specified
  if (typeof(round) == "double") res <- round(res, round)

  # return
  res
}




