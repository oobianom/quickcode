#' Listing of all CRAN archived R packages
#'
#' Retrieve a list of all currently archived R packages
#'
#' @param startsWith one letter that the package name starts with eg. a, e, f
#'
#' @return a data frame container listing of all archived R packages
#'
#' @note
#' The startsWith argument should be one letter and should be in lowercase
#'
#'
#' @export
#'

archivedPkg <- function(startsWith = letters) {
  startsWith <- match.arg(startsWith)
  read.csv(
    file = paste0(
      "https://quickcode.obi.obianom.com/CRAN/archiveddata_",tolower(startsWith),".txt?count=0&auth=1"),
    header = TRUE, quote = "'",
    skip = 1)
}

