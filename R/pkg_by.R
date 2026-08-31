#' Fetch R package based on keyword
#'
#' This function gets R packages based on a keyword in their description.
#'
#' @param keyword character, the keyword to search package descriptions for.
#'
#' @return character vector of matching package names or NULL if no matches.
#'
#' @note
#' the function in its current form only searches available.packages()
#'
#' @examples
#' \donttest{
#' # find the list of R packages for data or machine learning
#'
#' matched_pkgs <- suppressWarnings(find_packages("plotting"))
#' matched_pkgs
#'
#' matched_pkgs <- suppressWarnings(find_packages("machine learning"))
#' matched_pkgs
#' }
#' @export


find_packages <- function(keyword) {
  options(repos = structure(c(CRAN = "https://cran.r-project.org")))
  packages <- rownames(utils::available.packages())

  matched_pkgs <- c()

  for (pkg in packages) {
    tryCatch({
      desc <- utils::packageDescription(pkg)$Description

      if (grepl(keyword, desc, ignore.case = TRUE)) {
        matched_pkgs <- c(matched_pkgs, pkg)
      }
    },
    error = function(e) {
    })
  }

  if (length(matched_pkgs) == 0) {
    return(NULL)
  } else {
    return(matched_pkgs)
  }
}
