#' R Color Constant
#'
#' R color constants
#'
#' @param title title of the output
#'
#'
#' @examples
#' rcolorconst()
#'
#'
#' @export

rcolorconst <- function(title = "R Color Constants"){
  rcolorconst = c("black" = 1, "red" = 2, "green" = 3, "blue" = 4, "cyan" = 5, "magenta" = 6, "orange" = 7, "gray" = 8)
  Polychrome::swatch(rcolorconst, main = title)
}


 # rcolorconst = c(
 #    "black" = "#000000",
 #    "red" = "#FF0000",
 #    "green" = "#00FF00",
 #    "blue" = "#0000FF",
 #    "cyan" = "#00FFFF",
 #    "magenta" = "#FF00FF",
 #    "orange" = "#FFA500",
 #    "gray" = "#BEBEBE")
 #
 #  labelcols <- c("white", "black")[c(1,2,2,1,2,2,2,2)]
 #  L <- length(rcolorconst)
 #  pts <- graphics::barplot(rep(1, L), col = rcolorconst, main = title, yaxt = "n")
 #  graphics::text(pts, 0.5, names(rcolorconst), srt = 90, col = labelcols)
 #  invisible(pts)

