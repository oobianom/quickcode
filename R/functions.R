#' Not numeric
#'
#' Check if entry is not numeric
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is numeric
#' @examples
#' not.numeric("45") # TRUE
#' not.numeric(45) # FALSE
#' if(not.numeric(45)) print("yes") # yes
#'
#' @export

not.numeric <- function(x) !is.numeric(x)


#' Not an integer
#'
#' Check if entry is not an integer
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is an integer
#' @examples
#' not.integer(23.43) # TRUE
#' not.integer(45L) # FALSE
#' if(not.integer(4L)) print("yes") # NULL
#'
#' @export

not.integer <- function(x) !is.integer(x)


#' Not an environment
#'
#' Check if entry is not an environment object
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is an environment
#' @examples
#' test.env <- new.env()
#' test.notenv <- list(t=1)
#' not.environment(test.env) # FALSE
#' not.environment(test.notenv) # TRUE
#' if(not.environment(test.notenv)) print("yes") # yes
#'
#' @export

not.environment <- function(x) !is.environment(x)


#' Not a data
#'
#' Check if entry is not a data object
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is a data table
#' @examples
#' test.dt <- data.frame(ID=1:200,Type="RPKG.net")
#' test.notenv <- list(t=1)
#' not.data(test.dt) # FALSE
#' not.data(test.notenv) # TRUE
#' if(not.data(test.dt)) print("yes") # NULL
#'
#' @export

not.data <- function(x) !is.data.frame(x)


#' Load specific R libraries and clear environment
#'
#' Only include libraries, don't install if library doesn't exist
#'
#' @param x multiple library names
#' @return loaded libraries and clear environment
#' @examples
#' \dontrun{
#' libraryAll() # show installed libraries
#' libraryAll(r2symbols,dplyr,ggplot2,shinyStorePlus)
#' libraryAll("r2ymbols")
#' }
#' @export

libraryAll <- function(..., lib.loc = NULL, quietly = FALSE, clear = TRUE) {
  lib.names <- as.list(substitute(args(...))[-1L])
  lapply(lib.names, function(lib) do.call("library", list(package = lib, lib.loc = lib.loc, quietly = quietly)))

  if(!length(lib.names)){
    installed.packages()
  }
  if(clear)erase("\014")
}

#' function to calculate geometric mean

mean.gm <- function(x, na.rm = TRUE) {
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}



#' function to calculate geometric sd

#' @export

sd.gm <- function(x, na.rm = TRUE) {
  exp(sd(log(x[x > 0]), na.rm = na.rm))
}



#' function to calculate geometric percent CV
#' @export

cv.gm <- function(x, na.rm = TRUE, `%` = TRUE) {
  res <- sqrt(exp(sd(log(x[x > 0]), na.rm = na.rm)^2) - 1)
  if (`%`) res <- res * 100
  res
}


#' Add to a vector just like array_push in php
#' @export
#'
vector_push <- function(., add) {
  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  assign(as.character(..), c(get(.., envir = parent.frame()), add), envir = parent.frame())
}


#' Add to a data just like array_push in php
#' @export
#'


data_push <- function(., add, which = c("rows", "cols")) {
  which <- match.arg(which)
  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  data <- as.data.frame(get(.., envir = parent.frame()))
  add <- as.data.frame(add)
    switch(which,
           "rows" = {
             data <- rbind(data,add)
           },
           "cols" = {
             data <- cbind(data,add)
           }
    )
  assign(as.character(..), data, envir = parent.frame())
}





#' Shuffle a vector just like shuffle in php

#' @export

#'



vector_shuffle <- function(., replace = FALSE, prob = NULL) {
  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  val <- get(.., envir = parent.frame())

  assign(as.character(..), sample(val, length(val), replace = replace, prob = prob), envir = parent.frame())
}



#' Shuffle a dataframe just like shuffle but for data frame

#' @export

#'



data_shuffle <- function(., which = c("rows", "cols")) {
  which <- match.arg(which)

  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  data <- as.data.frame(get(.., envir = parent.frame()))

    switch(which,
      "rows" = {
        data <- data[sample(nrow(data)), ]
      },
      "cols" = {
        data <- data[, sample(ncol(data))]
      }
    )

  assign(as.character(..), data, envir = parent.frame())
}



#' clear console, clear environment, and setwd

#' @export



clean <- function(setwd = NULL, source = c(), load = c()) {
  # clear console, clean garbage and shut devices

  erase("\014")

  rm(list = setdiff(ls(envir = parent.frame()), c("setwd", "source", "load")),envir = parent.frame())

  graphics.off()

  gc()



  # set directory if it exists

  if ((!is.null(setwd)) & dir.exists(setwd)) {
    setwd(setwd)
  }



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



#' @export

`%!in%` <- function(x, table) {
  !(x %in% table)
}



#' clear console, clear environment, and setwd

#' @export

refresh <- clean







#' @export

libraryOne <- function(..., lib.loc = NULL, quietly = FALSE) {
  # Purpose: include all needed libraries with one call

  # 1: Get the current document

  # 2: Extract all the functions within the document

  # 3: For each of the functions, find the packages

  # 4: match the most likely packages and import

  # 5: reshape text to indicate included packages
}







#' @export

insertInText <- function(string) {
  adc <- rstudioapi::getActiveDocumentContext()

  rstudioapi::insertText(location = adc$selection[[1]]$range$start, string)
}



#' @export

header <- function() {
  insertInText(paste0("

  ############################################################################
  #  Document Path: ", rstudioapi::getActiveDocumentContext()$path, "
  #
  #  Written by USERNAME on ", Sys.Date(), "
  #
  #  R Version: ", version$version.string, "
  #
  #############################################################################

  "))
}



#' @export

header.rmd <- function() {
  insertInText(paste0("<!---

   Document Path: ", rstudioapi::getActiveDocumentContext()$path, "

   Written by USERNAME on ", Sys.Date(), "

   R Version: ", version$version.string, "

  -->"))
}



#' @export

requireAll <- function(..., lib.loc = NULL, quietly = TRUE) {
  lib.names <- as.list(substitute(args(...))[-1L])

  lapply(as.character(lib.names), function(lib) {
    if (length(lib)) {
      if (!try(require(lib, lib.loc = lib.loc, quietly = TRUE))) {
        if (install.packages(lib)) {
          if (!try(require(lib, lib.loc = lib.loc, quietly = quietly))) {
            warning(paste0(lib, " package does not exist"))
          }
        }
      }
    }
  })
}









#' @export

sample_by_column <- function(.dt, col, n) {
  .dt[.dt[, as.character(substitute(col))] %in% sample(unique(.dt[, as.character(substitute(col))]), n), ]
}







#' @export

has <- function(., var, col, row) {
  stop("Not completed")

  object <- .

  if (is.data.frame(object)) {
    # check if var exists in data.frame



    # check if column exists in data.frame



    # check if row exists in data.frame
  }



  if (is.vector(object)) {
    # check if var exists in vector
  }



  if (is.character(object)) {
    # check if var exists in character
  }



  if (is.numeric(object)) {
    # check if var exists in number
  }
}

# shorthand print to erase
erase <- cat
