#' Load specific R libraries and clear environment
#'
#' Load specific packages, print a list of the loaded packages along with versions.
#' Only include libraries, don't install if library doesn't exist
#'
#' @param ... multiple library names
#' @param lib.loc OPTIONAL. library store location
#' @param quietly OPTIONAL. attach library quietly
#' @param clear OPTIONAL. clear environment after attach
#' @param clearPkgs Clear previous loaded packages, TRUE or FALSE
#' @return loaded libraries and clear environment
#' @examples
#' \donttest{
#' # load packages and print their versions to the console
#' libraryAll(base) #one package
#'
#' libraryAll(
#'   base,
#'   tools,
#'   stats
#' ) #multiple packages
#'
#' libraryAll("grDevices") #with quotes
#'
#' libraryAll(
#'   stats,
#'   utils,
#'   quietly = TRUE
#' ) #load quietly
#'
#' libraryAll(
#'   base,
#' clear = FALSE) #do not clear console after load
#'
#' # clear previously loaded packages, then load r2resize and r2social
#' libraryAll(
#'   stats,
#'   utils,
#'   clearPkgs = TRUE
#' )
#' }
#' @export

libraryAll <- function(..., lib.loc = NULL, quietly = FALSE, clear = TRUE, clearPkgs = FALSE) {
  # remove previous loaded packages
  if (clearPkgs) {
    clearPreviouslyLoadedPkgs()
  }
  # load quickcode if not loaded
  if("quickcode" %nin% (.packages())) library(quickcode, quietly = TRUE)
  # load user requested libraries
  lib.names <- as.list(substitute(args(...))[-1L])
  if(!length(lib.names)) stop("Packages must be included to execute this function")
  invisible(lapply(lib.names, function(lib) do.call("library", list(package = lib, lib.loc = lib.loc, quietly = quietly, warn.conflicts = FALSE))))
  if(clear){
    erase()
    message("Packages Loaded:")
    #return package versions
    invisible(lapply(lib.names, function(lib)
      message(paste0(lib,", version ",utils::packageVersion(lib)))))
  }
}
