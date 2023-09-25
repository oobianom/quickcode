#' Listing of all CRAN archived R packages
#'
#' Retrieve a list of all currently archived R packages
#'
#' @param startsWith one letter that the package name starts with eg. a, e, f
#' @param inc.date should archive date be included in the result
#' @param as return result as data frame or as vector
#'
#' @return a data frame container listing of all archived R packages
#'
#' @note
#' The startsWith argument should be one letter and should be in lowercase
#'
#'
#' @export
#'

archivedPkg <- function(startsWith = letters, inc.date =TRUE, as =c("data.frame","vector")) {
  startsWith <- match.arg(startsWith)
  as <- match.arg(as)
  res <- read.csv(
    file = paste0(
      "https://quickcode.obi.obianom.com/CRAN/archiveddata_",tolower(startsWith),".txt?count=0&auth=1"),
    header = TRUE, quote = "'",
    skip = 1)

  if(!inc.date) res <- res[,1]
  if(as == "vector") res <- as.vector(res)
  res
}

