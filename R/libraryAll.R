#' Load specific R libraries and clear environment
#'
#' Only include libraries, don't install if library doesn't exist
#'
#' @param ... multiple library names
#' @param lib.loc OPTIONAL. library store location
#' @param quietly OPTIONAL. attach library quietly
#' @param clear OPTIONAL. clear environment after attach
#' @return loaded libraries and clear environment
#' @examples
#' \donttest{
#' libraryAll(r2resize) #one package
#'
#' libraryAll(
#'   r2symbols,
#'   dplyr,
#'   ggplot2,
#'   shinyStorePlus
#' ) #multiple packages
#'
#' libraryAll("r2ymbols") #with quotes
#'
#' libraryAll(
#'   PKNCA,
#'   mrgsolve,
#'   quietly = TRUE
#' ) #load quietly
#'
#' libraryAll(
#'   nextGenShinyApps,
#' clear = FALSE) #do not clear console after load
#'
#' }
#' @export

libraryAll <- function(..., lib.loc = NULL, quietly = FALSE, clear = TRUE) {
  # load quickcode if not loaded
  if("quickcode" %nin% (.packages())) library(quickcode, quietly = TRUE)
  # load user requested libraries
  lib.names <- as.list(substitute(args(...))[-1L])
  lapply(lib.names, function(lib) do.call("library", list(package = lib, lib.loc = lib.loc, quietly = quietly)))

  if(!length(lib.names)){
    library()
  }
  if(clear)erase()
}
