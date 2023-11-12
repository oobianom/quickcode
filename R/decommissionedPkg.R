#' Check whether an R package has been decommissioned in CRAN
#'
#' Designed to assist users in checking the decommission status
#' of an R package on CRAN. In the context of R language,
#' CRAN stands for the Comprehensive R Archive Network.
#'
#' @param package package name to query
#'
#' @return the decommissioned status of a particular package
#' based on the available packages using the utils package
#'
#' @details
#' CRAN is a network of servers around the world that store R packages
#' and their documentation, providing a centralized repository for
#' the R community. With the current function, users can quickly and
#' easily determine whether a specific R package has been decommissioned
#' on CRAN, ensuring they stay informed about the availability and
#' support status of the packages they rely on for their R
#' programming projects. This tool simplifies the process of package
#' management, helping users maintain up-to-date and reliable
#' dependencies in their R code.
#'
#'
#' @examples
#' # check if cattonum package is decomissioned
#' # the current package is expected to be decommissioned
#' rdecomPkg("cattonum")
#'
#' # check if dplyr is decommissioned
#' # the current package is expected NOT to be decommissioned
#' rdecomPkg("dplyr")
#'
#'
#' @export

rDecomPkg <- function(package){
  unlist(lapply(package, function(.p) .p %nin% allCRANpkg()))
}




