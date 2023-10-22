#' Compare histograms of two distributions
#'
#' Given two distributions, compare values from two histograms
#'
#' @param x1 numeric distribution 1
#' @param x2 numeric distribution 2
#' @param title title of the hostogram plot
#' @param color color vector for output
#' @return return histogram comparison using basic histogram plot
#'
#' @examples
#' # compare two normal distributions
#' compHist(
#'   x1 = rnorm(1000, mean = 0),
#'   x2 = rnorm(1000, mean = 2),
#'   title = "Histogram of rnorm Distributions With Means 0 & 2",
#'   color = c("lightslateblue", "salmon", "mediumvioletred")
#' )
#'
#' @export

compHist <- function(x1, x2, title, color = c("green", "black", "yellow"), xlab = "", ylab = "", separate = FALSE) {

  message("Function is still under development. Please do not use.")

  # compute means, min and max
  meanx1 <- round(mean(x1),1)
  meanx2 <- round(mean(x2),1)
  x1x2 <- c(x1, x2)
  minx <- min(x1x2) - 0.1 * min(x1x2)
  maxx <- max(x1x2) + 0.1 * max(x1x2)

  # close devices if open
  dev.off()

  # check if plots should be separated
  if(separate) par(mfrow=c(1,2))
  if(separate & length(title) != 2)
    stop("Title must contain two titles if the plots are to be separated")


  # make plots
  hist(x1,
       main = ifelse(separate,title[1],title),
       xlab = xlab,
       ylab = ylab,
       col = rgb(0, 0, 1, alpha = 0.6),
       xlim = c(minx, maxx)
  )

  hist(x2,
       main = ifelse(separate,title[2],title),
       xlab = xlab,
       ylab = ylab,
       col = rgb(1, 0, 0, alpha = 0.6),
        add = ifelse(separate,FALSE,TRUE)
  )

  # add legend if the plot is combined
  if(!separate) legend("topright",
         legend = c(paste0("Mean: ", meanx1), paste0("Mean: ", meanx2), "Overlap"),
         fill = color
  )


}
