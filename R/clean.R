#' Clear environment, clear console, set work directory and load files
#'
#' Shorthand to quickly clear console, clear environment, set working directory, load files
#'
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
#' \dontrun{
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
#'
#' @export
#'



clean <- function(setwd = NULL, source = c(), load = c(), clearPkgs = FALSE) {
  # clear console, clean garbage and shut devices
  erase() #clear console
  rm(list = setdiff(ls(envir = parent.frame(),all.names = TRUE),
                    c("setwd", "source", "load", "clearPkgs")), envir = parent.frame())
  graphics.off() #graphics off
  closeAllConnections() #close any open connections
  gc() #garbage cleanup to free memory


  # set directory if it exists
  prevwd <- getwd()
  #on.exit(setwd(prevwd))

  if (not.null(setwd)) {
    if (dir.exists(setwd)) {
      setwd(setwd)
    }
  }

  # remove previous loaded packages
  if (clearPkgs) {
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

  # load quickcode if not loaded
  if ("quickcode" %nin% (.packages()))
    library(quickcode, quietly = TRUE)

  # source in any required files
  if (length(source)) {
    for (sourced in source) {
      if (file.exists(sourced)) source(sourced)
    }
  }

  # load in any required data
  if (length(load)) {
    for (loaded in load) {
      if (file.exists(loaded)) load(loaded, envir = parent.frame())
    }
  }
}



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
#' \dontrun{
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
#'
#' @export
#'
refresh <- clean

