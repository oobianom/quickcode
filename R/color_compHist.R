#' Compare histograms of two distributions
#'
#' For comparing histograms of two data distributions.
#' Simply input the two distributions, and it generates a clear and
#' informative histogram that illustrates the differences between the data.
#'
#' @param x1 NUMERIC. the first distribution
#' @param x2 NUMERIC. the second distribution
#' @param title CHARACTER. title of the histogram plot
#' @param col1 CHARACTER. color fill for first distribution
#' @param col2 CHARACTER. color fill for second distribution
#' @param xlab CHARACTER. label of the x-axis
#' @param ylab CHARACTER. label of the y-axis
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
#' @section Some recommended color pairs:
#' col1 = 'dodgerblue4' (and) col2 = 'darksalmon' \cr
#' col1 = 'brown' (and) col2 = 'beige' \cr
#' col1 = 'pink' (and) col2 = 'royalblue4' \cr
#' col1 = 'red' (and) col2 = 'yellow' \cr
#' col1 = 'limegreen' (and) col2 = 'blue' \cr
#' col1 = 'darkred' (and) col2 = 'aquamarine4' \cr
#'
#'
#' @examples
#' # compare two normal distributions with means that differ a lot
#' # in this case, the overlap will not be observed
#' set.seed(123)
#' compHist(
#'   x1 = rnorm(1000, mean = 3),
#'   x2 = rnorm(1000, mean = 10),
#'   title = "Histogram of Distributions With Means 3 & 10",
#'   col1 = "yellow", col2 = "violet"
#' )
#'
#'
#' # compare two normal distributions with means that are close
#' # in this case, the overlap between the histograms will be observed
#' set.seed(123)
#' compHist(
#'   x1 = rnorm(1000, mean = 0),
#'   x2 = rnorm(1000, mean = 2),
#'   title = "Histogram of rnorm Distributions With Means 0 & 2",
#'   col1 = "lightslateblue", col2 = "salmon"
#' )
#'
#' set.seed(123)
#' # separate the plots for preview
#' compHist(
#'   x1 = rnorm(1000, mean = 0),
#'   x2 = rnorm(1000, mean = 2),
#'   title = c("Plot Means 0", "Plot Means 2"),
#'   col1 = "#F96167", col2 = "#CCF381",
#'   separate = TRUE
#' )
#'
#' @export

compHist <- function(x1, x2, title, col1 = "red", col2 = "yellow", xlab = "", ylab = "Frequency", separate = FALSE) {
  # compute means, min and max
  meanx1 <- round(mean(x1), 1)
  meanx2 <- round(mean(x2), 1)
  x1x2 <- c(x1, x2)
  minx <- min(x1x2) - 0.1 * min(x1x2)
  maxx <- max(x1x2) + 0.1 * max(x1x2)

  # close devices if open
  # if (.Device != "null device") grDevices::dev.off()

  # check if plots should be separated
  if (separate) graphics::par(mfrow = c(1, 2))
  if (separate & length(title) != 2) {
    stop("Title must contain two titles if the plots are to be separated")
  }


  # make plots
  cl1 <- grDevices::col2rgb(col1) / 255
  cl1b <- grDevices::rgb(cl1[1, 1], cl1[2, 1], cl1[3, 1], alpha = 0.6)
  graphics::hist(x1,
    main = ifelse(separate, title[1], title),
    xlab = xlab,
    ylab = ylab,
    col = cl1b,
    xlim = c(minx, maxx)
  )

  cl2 <- grDevices::col2rgb(col2) / 255
  cl2b <- grDevices::rgb(cl2[1, 1], cl2[2, 1], cl2[3, 1], alpha = 0.6)
  graphics::hist(x2,
    main = ifelse(separate, title[2], title),
    xlab = xlab,
    ylab = ylab,
    col = cl2b,
    xlim = c(minx, maxx),
    add = ifelse(separate, FALSE, TRUE)
  )

  # add legend if the plot is combined
  if (!separate) {
    graphics::legend("topright",
      legend = c(paste0("Mean: ", meanx1), paste0("Mean: ", meanx2), "Overlap"),
      fill = c(cl1b, cl2b, mix.color(c(col1, col2), 2, 1))
    )
  }
}


#' Mix or Blend two or more colors
#'
#' Combine colors to generate a new color
#'
#' @param color color vector e.g see example
#' @param type return type of the output
#' @param alpha alpha or opacity of the resulting color
#' @return hex for the combined color
#' @examples
#' # color vector
#' colvec <- c("red", "blue", "violet", "green", "#ff0066")
#'
#' # just one color
#' mix.color(colvec[1], type = 1, alpha = 1)
#'
#' # add two colors
#' mix.color(colvec[1:2], type = 1, alpha = 1)
#'
#' # add three colors
#' mix.color(colvec[1:3], type = 1, alpha = 1)
#'
#'
#' # return type = 2
#'
#' # just one color
#' mix.color(colvec[1], type = 2, alpha = 1)
#'
#' # add two colors
#' mix.color(colvec[1:2], type = 2, alpha = 1)
#'
#' # add three colors
#' mix.color(colvec[1:3], type = 2, alpha = 1)
#'
#'
#' # opacity or alpha  0.5
#'
#' # just one color
#' mix.color(colvec[1], type = 1, alpha = 0.5)
#'
#' # add two colors
#' mix.color(colvec[1:2], type = 1, alpha = 0.5)
#'
#' # add three colors
#' mix.color(colvec[1:3], type = 1, alpha = 0.5)
#'
#' # add all colors
#' mix.color(colvec, type = 1, alpha = 0.5)
#'
#' @export
mix.color <- function(color, type = 2, alpha = 1) {
  stopifnot(alpha <= 1, alpha >= 0, type <= 3, type >= 1)
  vals <- apply(grDevices::col2rgb(color), 1, mean)
  switch(type,
    "1" = grDevices::rgb(vals[1], vals[2], vals[3], alpha * 255, maxColorValue = 255),
    "2" = grDevices::rgb(vals[1] / 255, vals[2] / 255, vals[3] / 255, alpha = alpha),
    "3" = vals
  )
}

#' Mix or Blend colors between two colors
#'
#' Mix or blend multiple colors between two colors
#' @param colors the vector of two colors
#' @param max maximum number of colors to blend between
#' @param alpha alpha for the new color blends
#' @param preview LOGICAL. preview all color generated
#'
#' @examples
#' # simple mix
#' mix.cols.btw(c("red","brown"))
#'
#' # also preview after mixing
#' mix.cols.btw(c("red","green"),alpha = 0.2, preview = T)
#'
#' mix.cols.btw(c("red","purple","yellow","gray"),alpha = 0.2, preview = T)
#'
#'
#'
#' @export
mix.cols.btw <- function(colors, max = 20, alpha = 1, preview = F) {
  message("This function is currently under development.")
  repeat{
    colors <- unlist(lapply(split(colors, ceiling(seq_along(colors) / 2)), function(ol) {
      if (length(ol) > 1) {
        nwcol <- mix.color(ol, alpha = alpha)
        append(ol, nwcol, 1)
      } else {
        ol
      }
    }))
    if (length(colors) >= max) break
  }


  # preview the colors generated sing swatch
  if (preview) {
    cls <- as.character(colors)
    names(cls) <- cls
    Polychrome::swatch(cls, main = "Preview of color mix")
  }

  # return color
  as.character(colors)
}
