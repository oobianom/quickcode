#' quickcode
#'
#' @name quickcode
#' @docType package
#' @importFrom grDevices graphics.off
#' @importFrom stats sd
#' @noRd
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname,pkgname){
  utils::globalVariables(c("graphics.off","vali", "sd", "x",'pattern','replacement'))
}

