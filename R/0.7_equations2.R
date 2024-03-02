#' Calculate the distance of points from the center of a cluster
#'
#' This function operates on multivariate data and
#' calculates the distance of points from the centroid of one or more clusters.
#'
#' @rdname distance
#' @param data dataset with at least 2 columns
#' @param round round result to decimal place
#' @return a named vector consisting of a row number and a pair-distance value
#'
#' @section Function utility:
#' Used to generate the computations needed to model pair-distance measures in three dimensions
#'
#' @section More information about this function:
#' The pairDist function is used to quantify how far each data point (row)
#' is from the overall mean across all columns. It’s commonly used in multivariate statistics,
#' machine learning, and data analysis to assess the variability or similarity of data points
#' relative to their mean. More specifically, the function is used in outlier detection and
#' cluster analysis to evaluate the dispersion of data. Used in conjunction with other
#' calculations, pairDist output can also be used to model data in three dimensions.
#'
#'
#' @references the current function was adapted from one of the examples in the svgViewR package, \cr
#' https://cran.r-project.org/web/packages/svgViewR/svgViewR.pdf
#' @examples
#' data = attenu[,1:2]
#'
#' #basic example
#' pairDist(data)
#'
#'
#' # round results to 2 decimal points
#' pairDist(data, 2)
#'
#' @export

pairDist <- function(data, round) {
  col_means <- colMeans(data)
  mean_matrix <- matrix(col_means, nrow = nrow(data), ncol = ncol(data), byrow = TRUE)
  squared_diff <- (data - mean_matrix)^2
  row_sums <- rowSums(squared_diff)
  result <- sqrt(row_sums)
  if (!missing(round)) result <- round(result, round)
  result
}


 # # check entry arguments
 #  if(ncol(data.frame(data)) %% n > 0)
 #    stop("n must be a multiple of data length e.g. n = ",ncol(data.frame(data))," or ",ncol(data.frame(data))*2," etc")
 #
 #  # check dataset to make sure it had at least 2 columns
 #  if(typeof(data) == "list")stopifnot(ncol(data) >= 2)
 #
 #  # compute based on what is provided
 #  if (missing(n)) {
 #    res <- sqrt(rowSums((data - matrix(colMeans(data)))^2))
 #  } else {
 #    # validate value of n
 #    stopifnot(n >= 1, n <= nrow(data))
 #    res <- sqrt(rowSums((data - matrix(colMeans(data), n, byrow = TRUE))^2))
 #  }
 #
 #  # round result if specified
 #  if (not.null(round)) res <- round(res, round)
 #
 #  # return
 #  res
 #

