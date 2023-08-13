#' Clear environment, clear console, set work directory and load files
#'
#' Shorthand to quickly clear console, clear environment, set working directory, load files
#'
#' @param setwd OPTIONAL. set working directory
#' @param source OPTIONAL. source in file(s)
#' @param load OPTIONAL. load in Rdata file(s)
#' @param clearPkgs Clear previous loaded packages, TRUE or FALSE
#' @return cleared environment and set directory
#'
#' @examples
#' \donttest{
#' quickcode::clean()
#' quickcode::clean(clearPkgs = TRUE) #clear all previously loaded packages
#' quickcode::clean(setwd = "/home/") #clear env and set working directory
#' quickcode::clean(source = c("/home/file1.R","file2"))
#' quickcode::clean(setwd = "/home/",source = c("file1","file2))
#' quickcode::clean(setwd = "/home/",source="file1.R",load="obi.RData")
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
  assign("last.warning",NULL, envir = baseenv()) #NULL last warnings
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
#' @examples
#' \donttest{
#' refresh()
#' refresh(setwd = "home")
#' refresh(setwd = "home",source = c("home/file1.R","file2"))
#' refresh(setwd = "home/",source = c("file1","file2))
#' refresh(setwd = "home/",source="file1.R",load="obi.RData")
#' }
#'
#' @export
#'
refresh <- clean

