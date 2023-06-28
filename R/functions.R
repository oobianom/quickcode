#' @export

not.numeric <- function(x) !is.numeric(x)



#' @export

not.integer <- function(x) !is.integer(x)



#' @export

not.environment <- function(x) !is.environment(x)



#' @export

not.data <- function(x) !is.data.frame(x)



#' @export

library <- function(..., lib.loc = NULL, quietly = FALSE) {
  lib.names <- as.list(substitute(args(...))[-1L])

  lapply(lib.names, function(lib) do.call("library", list(package = lib, lib.loc = lib.loc, quietly = quietly)))

  cat()
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



#' function to calculate geometric %CV

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

  assign(as.character(..), c(get(..), add), envir = parent.frame())
}





#' Shuffle a vector just like shuffle in php

#' @export

#'



vector_shuffle <- function(., replace = FALSE, prob = NULL) {
  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  val <- get(..)

  assign(as.character(..), sample(val, length(val), replace = replace, prob = prob), envir = parent.frame())
}



#' Shuffle a dataframe just like shuffle but for data frame

#' @export

#'



data_shuffle <- function(., which = c("rows", "cols")) {
  which <- match.arg(which)

  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  data <- get(..)

  data <-
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

  cat("\014")

  rm(list = setdiff(ls(), c("setwd", "source", "load")))

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
