#' R Color Constant
#'
#' R color constants description
#'
#' @param title title of the output
#'
#' @return returns color constant
#'
#' @details
#' Additional details...
#'
#' @section Use case:
#'
#' @examples
#' # Without title
#' rcolorconst()
#'
#' # With title
#' rcolorconst("My new color constant")
#'
#' @export

rcolorconst <- function(title = "R Color Constants"){
  rcolorconst = c("1" ="black",
                  "2" ="red",
                  "3" = "green",
                  "4" = "blue",
                  "5" = "cyan",
                  "6" = "magenta",
                  "7" = "orange",
                  "8" = "gray")
  Polychrome::swatch(rcolorconst, main = title)
  setNames(as.numeric(names(rcolorconst)), rcolorconst)
}

