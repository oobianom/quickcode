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
#' libraryAll() # show installed libraries
#' libraryAll(r2symbols,dplyr,ggplot2,shinyStorePlus)
#' libraryAll("r2ymbols")
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
