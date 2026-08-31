#' Clear environment, clear console, set work directory and load files
#'
#' Shorthand to quickly clear console, clear environment, set working directory, load files
#' @rdname clearenvironment
#' @param setwd OPTIONAL. set working directory
#' @param source OPTIONAL. source in file(s)
#' @param load OPTIONAL. load in Rdata file(s)
#' @param clearPkgs Clear previous loaded packages, TRUE or FALSE
#' @return cleared environment and set directory
#' @details
#' The purpose of this function is provide a one-line code to clear the console, clear the environment,
#' set working directory to a specified path, source in various files into the current file, and
#' load RData files into the current environment. The first process in the sequence of events is to clear the
#' environment. Then the working directory is set, prior to inclusion of various files and RData. With the directory
#' being set first, the path to the sourced in or RData files will not need to be appended to the file name. See examples.
#'
#' @examples
#' \donttest{
#' if(interactive()){
#' #simply clear environment, clear console and devices
#' quickcode::clean()
#'
#' #clear combined with additional arguments
#' quickcode::clean(
#'   clearPkgs = FALSE
#' ) #also clear all previously loaded packages if set to true
#'
#' quickcode::clean(
#'   setwd = "/home/"
#' ) #clear env and also set working directory
#'
#'
#' quickcode::clean(
#'   source = c("/home/file1.R","file2")
#' ) #clear environment and source two files into current document
#'
#'
#' quickcode::clean(
#'   setwd = "/home/",
#'   source = c("file1","file2")
#' ) #clear environment, set working directory and source 2 files into environment
#'
#'
#' quickcode::clean(
#'   setwd = "/home/",
#'   source="file1.R",
#'   load="obi.RData"
#' ) #clear environment, set working directory, source files and load RData
#' }
#' }
#' @export
#'



clean <- function(setwd = NULL, source = c(), load = c(), clearPkgs = FALSE) {
  # clear console, clean garbage and shut devices
  erase() #clear console
  rm(list = setdiff(ls(envir = parent.frame(),all.names = TRUE),
                    c("setwd", "source", "load", "clearPkgs")), envir = parent.frame())
  #graphics off
  if(.Device !="null device" | length(grDevices::dev.list())) graphics.off()
  #close any open connections
  options(NULL)
  close(file())
  #garbage cleanup to free memory
  gc()


  # set directory if it exists
  prevwd <- getwd()
  options("last.used.wd" = prevwd)
  #on.exit(setwd(prevwd))

  if (not.null(setwd)) {
    if (dir.exists(setwd)) {
      setwd(setwd)
    }
  }

  # remove previous loaded packages
  if (clearPkgs) {
    clearPreviouslyLoadedPkgs()
  }

  # load quickcode if not loaded
  if ("quickcode" %nin% (.packages()))
    library(quickcode, quietly = TRUE)

  # source in any required files
  if (length(source)) {
    for (sourced in source) {
      message("Importing ",sourced)
      if (file.exists(sourced)) source(sourced)
      else warning(sourced," does not exist.\n")
    }
  }

  # load in any required data
  if (length(load)) {
    for (loaded in load) {
      message("Loading ",loaded)
      if (file.exists(loaded)) load(loaded, envir = parent.frame())
      else warning(loaded," does not exist.\n")
    }
  }
}

#' Go back to previous directory
#'
#' Navigate to previous working directory if the setwd was called by clean() or refresh() function
#'
#' @note
#' In order to use this function to retrieve the last/previous working directory, you must use the quickcode::clean() or quickcode::refresh() function to set the working directory.
#' @examples
#' lastwd()
#' @return the previous directory
#' @export

lastwd <- function() {
  .lastdir <- options()$last.used.wd
  if (is.null(.lastdir)) {
    message("In order to use this function to retrieve the last/previous working directory, you must use the quickcode::clean() or quickcode::refresh() function to set the working directory.")
  } else {
    options("last.used.wd" = getwd())
    if(dir.exists(.lastdir)) setwd(.lastdir)
    else message("The last directory (",.lastdir,") no longer exists.")
  }
}
#' @export
#' @rdname clearenvironment
clear <- clean

#' Clear environment, clear console, set work directory and load files
#'
#' Shorthand to quickly clear console, clear environment, set working directory, load files
#'
#' @param setwd OPTIONAL. set working directory
#' @param source OPTIONAL. source in file(s)
#' @param load OPTIONAL. load in Rdata file(s)
#' @param clearPkgs clear previously loaded packages
#' @return cleared environment and set directory
#' @details
#' The purpose of this function is provide a one-line code to clear the console, clear the environment,
#' set working directory to a specified path, source in various files into the current file, and
#' load RData files into the current environment. The first process in the sequence of events is to clear the
#' environment. Then the working directory is set, prior to inclusion of various files and RData. With the directory
#' being set first, the path to the sourced in or RData files will not need to be appended to the file name. See examples.
#' @examples
#' \donttest{
#' if(interactive()){
#' #exactly like the clean function
#' #simply clear environment, clear console and devices
#' quickcode::refresh()
#'
#' #clear combined with additional arguments
#' quickcode::refresh(
#'   clearPkgs = FALSE
#' ) #also clear all previously loaded packages if set to TRUE
#'
#' quickcode::refresh(
#'   setwd = "/home/"
#' ) #clear env and also set working directory
#'
#'
#' quickcode::refresh(
#'   source = c("/home/file1.R","file2")
#' ) #clear environment and source two files into current document
#'
#'
#' quickcode::refresh(
#'   setwd = "/home/",
#'   source = c("file1","file2")
#' ) #clear environment, set working directory and source 2 files into environment
#'
#'
#' quickcode::refresh(
#'   setwd = "/home/",
#'   source="file1.R",
#'   load="obi.RData"
#' ) #clear environment, set working directory, source files and load RData
#'
#' }
#' }
#' @export
#'
refresh <- clean


clearPreviouslyLoadedPkgs <- function(){
  deftPkg <- c("base", "quickcode", getOption("defaultPackages"))
  for (i in grep("package:", search(), value = TRUE)) {
    curr <- strsplit(i, ":")[[1]][2]
    if (curr %nin% deftPkg){
      tryCatch({
        detach(name = i, character.only = TRUE, force = TRUE)
      }, warning = function(w) {},
      error = function(e) {},
      finally = {})

    }
  }
}
