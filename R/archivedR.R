#' Listing of all CRAN archived R packages
#'
#' Retrieve a list of all currently archived R packages
#'
#' @param n number of packages to retrieve
#'
#' @return a data frame container listing of all archived R packages
#'
#' @note
#' If the n argument is set to 0, all packages will be retrieved. Keep in mind that the larger the count or if all packages
#' are requested, this function becomes slower.
#'
#'
#' @export
#'

archivedPkg <- function(n = 0) {
  read.csv(
    file = paste0(
      "https://quickcode.obi.obianom.com/CRAN/searchArchivedCRANPkg.php?count=",
      as.integer(n)),
    header = TRUE,
    skip = 1)
}
