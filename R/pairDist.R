#' Calculate the distance of points from the center of a cluster
#'
#' This function operates on multivariate data and
#' calculates the distance of points from the centroid of one or more clusters.
#'
#' @rdname machine_learning
#' @param data data frame object or a matrix/array object
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
#'
#' data = attenu[,1:2]
#'
#' # example 1: basic example using data.frame
#' pairDist(data)
#'
#' # example 2: basic example using as.matrix
#' pairDist(as.matrix(data))
#'
#' # example 3: round results to 2 decimal points
#' pairDist(data, 2)
#'
#' # example 4
#' data = matrix(
#' c(1, 5, NA, 4, 5, 6, NA, 8, 9, NA, 12, 23, 6, 0, 3, 3, 8, 15),
#' ncol = 3,
#' byrow = TRUE)
#' x = pairDist(data, 3)
#' data = data.frame(data)
#' data = cbind(data, x)
#'
#' @export

pairDist <- function(data, round) {
  if (!all(unique(unlist(sapply(data, class))) %in% c("numeric", "double", "integer")))
    stop("The data should only include numeric values and NAs")

  # Compute the column means ignoring NA values
  mean_matrix <- matrix(colMeans(data, na.rm = TRUE), nrow = nrow(data), ncol = ncol(data), byrow = TRUE)

  # Compute squared differences ignoring NA values
  squared_diff <- (data - mean_matrix)^2
  row_sums <- rowSums(squared_diff, na.rm = TRUE)

  # Calculate the result
  result <- sqrt(row_sums)

  # Round the result if specified
  if (!missing(round))result <- round(result, round)

  # Apply function to check for NAs in each record
  data.frame(pdist = result, isNA = apply(data, 1, function(row) any(is.na(row))))
}


