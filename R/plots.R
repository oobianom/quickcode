#' Compare histograms of two distributions
#'
#' For comparing values between two distributions.
#' Simply input the two distributions, and it generates a clear and
#' informative histogram that illustrates the differences between the data.
#'
#' @param x1 numeric distribution 1
#' @param x2 numeric distribution 2
#' @param title title of the hostogram plot
#' @param color color vector of 2 for output
#' @param xlab label of the x-axis
#' @param ylab label of the y-axis
#' @param separate LOGICAL. whether to separate the plots
#' @return return histogram comparison using basic histogram plot
#'
#' @details
#' Users have the option to view individual histograms for each distribution
#' before initiating the comparison, allowing for a detailed examination of
#' each dataset's characteristics. This feature ensures a comprehensive
#' understanding of the data and enhances the user's ability to interpret
#' the results of the distribution comparison provided by this function.
#'
#' @examples
#' # compare two normal distributions
#'
#' set.seed(123)
#' compHist(
#'   x1 = rnorm(1000, mean = 0),
#'   x2 = rnorm(1000, mean = 2),
#'   title = "Histogram of rnorm Distributions With Means 0 & 2",
#'   color = c("lightslateblue", "salmon")
#' )
#'
#' set.seed(123)
#' # separate the plots for preview
#' compHist(
#'   x1 = rnorm(1000, mean = 0),
#'   x2 = rnorm(1000, mean = 2),
#'   title = c("Plot Means 0", "Plot Means 2"),
#'   color = c("lightslateblue", "blue"),
#'   separate = TRUE
#' )
#'
#' @export

compHist <- function(x1, x2, title, color = c("green", "black"), xlab = "", ylab = "", separate = FALSE) {

  # compute means, min and max
  meanx1 <- round(mean(x1), 1)
  meanx2 <- round(mean(x2), 1)
  x1x2 <- c(x1, x2)
  minx <- min(x1x2) - 0.1 * min(x1x2)
  maxx <- max(x1x2) + 0.1 * max(x1x2)

  # close devices if open
  if(.Device !="null device") grDevices::dev.off()

  # check if plots should be separated
  if (separate) graphics::par(mfrow = c(1, 2))
  if (separate & length(title) != 2) {
    stop("Title must contain two titles if the plots are to be separated")
  }


  # make plots
  cl1 <- grDevices::col2rgb(color[1])/255
  cl1b <- grDevices::rgb(cl1[1,1],cl1[2,1],cl1[3,1],alpha = 0.6)
  graphics::hist(x1,
    main = ifelse(separate, title[1], title),
    xlab = xlab,
    ylab = ylab,
    col = cl1b,
    xlim = c(minx, maxx)
  )

  cl2 <- grDevices::col2rgb(color[2])/255
  cl2b <- grDevices::rgb(cl2[1,1],cl2[2,1],cl2[3,1],alpha = 0.6)
  graphics::hist(x2,
    main = ifelse(separate, title[2], title),
    xlab = xlab,
    ylab = ylab,
    col = cl2b,
    xlim = c(minx, maxx),
    add = ifelse(separate, FALSE, TRUE)
  )

  d1 <-(colorspace::mixcolor(0.6,colorspace::sRGB(cl1[1],cl1[2],cl1[3]),colorspace::sRGB(cl2[1],cl2[2],cl2[3])))
  d1 <- as.numeric(d1@coords)

  # add legend if the plot is combined
  if (!separate) {
    graphics::legend("topright",
      legend = c(paste0("Mean: ", meanx1), paste0("Mean: ", meanx2), "Overlap"),
      fill = c(cl1b,cl2b,grDevices::rgb(d1[1],d1[2],d1[3],alpha = 0.6))
    )
  }
}
