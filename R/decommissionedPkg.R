allCRANpkg <- data.frame(utils::available.packages())$Package

#' Check whether an R package has been decomissioned in CRAN
#'
#' Check if an R package has been decommisioned in CRAN
#'
#' @param package package name to query
#'
#' @export

rdecomPkg <- function(package){
  unlist(lapply(package, function(.p) .p %in% allCRANpkg))
}
