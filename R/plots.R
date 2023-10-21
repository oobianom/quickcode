#' Compare two histograms
#'
#' Compare values from two histograms
#'
#' @param x1 histogram 1
#' @param x2 histograme 2
#' @return return histogram comparison
#'
#' @examples
#' # example code
#'
#'
#' @export

compHist <- function(x1 = rnorm(1000, mean = 0), x2 = rnorm(1000, mean = 2), main = "Histogram of rnorm Distributions With Means 0 & 2"){
message("Function is still underdevelopment. Please do not use.")

  set.seed(249)

  hist(x1,
       main = main,
       xlab = "",
       ylab = "",
       col = rgb(0, 0, 1, alpha = 0.6),
       xlim = c(minx, maxx))

  hist(x2, xlab = "",
       ylab = "",
       col = rgb(1, 0, 0, alpha = 0.6),
       add = TRUE)

  legend("topright",
         legend = c("Mean: 0", "Mean: 2", "Overlap"),
         fill = c("lightslateblue", "salmon","mediumvioletred"))

}
