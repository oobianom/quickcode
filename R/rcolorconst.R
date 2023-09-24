#' R Color Constant
#'
#' @param title title of the output
#'
#' @export

rcolorconst <- function(title = "R Color Constants"){
  rcolorconst = c(
    "black" = "#000000",
    "red" = "#FF0000",
    "green" = "#00FF00",
    "blue" = "#0000FF",
    "cyan" = "#00FFFF",
    "magenta" = "#FF00FF",
    "orange" = "#FFA500",
    "gray" = "#BEBEBE")
  luvmat <<- as(colorspace::hex2RGB(rcolorconst), "LUV")
  print(luvmat)
  x <- luvmat@coords
  labelcols <- c("white", "black")[1 + 1 * (x[,1] > 50)]
  L <- length(rcolorconst)
  pts <- barplot(rep(1, L), col = rcolorconst, main = title, yaxt = "n")
  text(pts, 0.5, names(rcolorconst), srt = 90, col = labelcols)
  invisible(pts)
}




