#' Check whether an R package has been decommissioned in CRAN
#'
#' Check if an R package has been decommisioned in CRAN
#'
#' @param package package name to query
#'
#' @examples
#' # check if package is decomissioned
#' rdecomPkg("cattonum")
#'
#' @export

rdecomPkg <- function(package){
  unlist(lapply(package, function(.p) .p %nin% allCRANpkg()))
}



allCRANpkg <- function(){
  utils::chooseCRANmirror(ind = 1)
  data.frame(utils::available.packages())$Package
}
