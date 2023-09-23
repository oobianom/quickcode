#' R Color Constant
#'
#' @export

rcolorconst <- function(){
  rcolorconst = c("black" = 1, "red" = 2, "green" = 3, "blue" = 4, "cyan" = 5, "magenta" = 6, "orange" = 7, "gray" = 8)
  rcolconst = c("black", "red", "green", "blue", "cyan", "magenta", "orange", "gray")
  Polychrome::swatch(rcolconst, main = "R Color Constants")
}
