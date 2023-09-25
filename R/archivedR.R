#' Listing of all CRAN archived R packages
#'
#' Retrieve a list of all currently archived R packages
#'
#' @param startsWith letter that the package name starts with
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

archivedPkg <- function(startsWith = letters) {
  startsWith <- match.arg(startsWith)
  read.csv(
    file = paste0(
      "https://quickcode.obi.obianom.com/CRAN/archiveddata_",tolower(startsWith),".txt?count="),
    header = TRUE,
    skip = 1)
}

#"https://quickcode.obi.obianom.com/CRAN/searchArchivedCRANPkg.php?count="
