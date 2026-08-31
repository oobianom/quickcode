#' Check whether an R package has been decommissioned in CRAN
#'
#' Designed to assist users in checking the decommission status
#' of an R package on CRAN. In the context of R language,
#' CRAN stands for the Comprehensive R Archive Network.
#'
#' @param package package name to query
#'
#' @return the decommissioned status of a particular package based on the available packages using the utils package
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
#' \dontrun{
#' # check if cattonum package is decommissioned
#' # the current package is expected to be decommissioned
#' rDecomPkg("cattonum")
#'
#' # check if dplyr is decommissioned
#' # the current package is expected NOT to be decommissioned
#' rDecomPkg("dplyr")
#'
#' # when a package never existed in CRAN
#' # the result of the function call should be NA
#' rDecomPkg("printy")
#' rDecomPkg("package0002312122312")
#' }
#' @export

rDecomPkg <- function(package){
  stopifnot(nchar(package) > 1) #stop if the package name length is not greater than 1

  #check if the package ever existed in cran
  if(pkg.existed.cran(package)){
    #if it existed, check if it is active
    unlist(lapply(package, function(.p) .p %nin% allCRANpkg()))
  }else{
    #if it did not exist, return a warning and NA
    warning(paste0("The package '",package,"' never existed in CRAN."))
    return(NA)
  }

}



