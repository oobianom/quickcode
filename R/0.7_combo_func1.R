# Combinatory functions

#' Split a string of numbers and return numeric
#'
#' Given a sting, split by a separator into numbers
#'
#' @inheritParams strsplit
#' @return numeric values based on split string
#' @export

strsplit.num <- function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE){
  as.numeric(unlist(strsplit(x, split, fixed, perl, useBytes)))
}



#' Split a string of values and return as boolean
#'
#' Given a sting, split by a separator into boolean
#'
#' @inheritParams strsplit
#' @param type type of return, see the as.boolean function for more info
#' @return boolean values based on split string
#' @export

strsplit.bool <- function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE,type = 2){
  as.boolean(unlist(strsplit(x, split, fixed, perl, useBytes)),type = type)
}




#' Read in a CSV and print first X rows and Columns
#'
#' Read a dataset of type csv and print x rows and y columns
#'
#' @inheritParams read.csv
#' @param type type of return, see the as.boolean function for more info
#' @return read csv content
#' @export

read.csv.print <- function(file, header = TRUE, sep = ",", quote = "\"", dec = ".",
                           fill = TRUE, comment.char = "", ...,dim = c(10L,5L)){
  x <- read.table.print(file = file, header = header, sep = sep, quote = quote,
                  dec = dec, fill = fill, comment.char = comment.char, dim = 0, ...)
  if(multiply(dim) > 0) print(x, dim)
  x
}

#' Read in a CSV and print first X rows and Columns
#'
#' Read a dataset of type csv and print x rows and y columns
#'
#' @inheritParams read.csv
#' @param type type of return, see the as.boolean function for more info
#' @return read table content
#' @export

read.table.print <- function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE,type = 2){
  x <- read.table(file = file, header = header, sep = sep, quote = quote,
                        dec = dec, fill = fill, comment.char = comment.char, dim = c(10,10), ...)
  if(n > 0) print(x, dim)
  x
}



#' Multiple a vector of numeric values
#'
#' Mulitple all the content of a vector
#'
#' @param ... the numeric values to multiply
#' @return multiple of all content
#' @export

multiply <- function(...)tail(cumprod(unlist(...)),n=1)

