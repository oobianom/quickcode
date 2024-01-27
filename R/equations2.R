#' Calculate the distance of points from the center of a cluster
#'
#' This function operates on multivariate data and
#' calculates the distance of points from the centroid of one or more clusters.
#'
#' @rdname distance
#' @param n Number of observations (scaler)
#' @param v A vector numerically referencing which variables to select;
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
#' #basic example
#' v= c(2,4:10)
#' n = 6
#' data =
#' pairdist(data, n, v)
#'
#'
#' @export

pairDist <- function(data, n, v, round = NULL) {
  message("This function is still being developed")
  res <- sqrt(rowSums((data- matrix(colMeans(data), n, v, byrow = TRUE))^2))
  if(typeof(round) == "integer") res <- round(res,round)
  res
}




