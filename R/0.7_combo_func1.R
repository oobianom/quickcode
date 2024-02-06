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
#' @param dim dimension of CSV content to print
#' @inheritParams read.csv
#'
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
#' @param dim dimension of table content to print
#' @inheritParams read.table
#'
#' @return read table content
#' @export

read.table.print <- function(file, header = FALSE, sep = "", quote = "\"'", dec = ".",
                             numerals = c("allow.loss", "warn.loss", "no.loss"), row.names,
                             col.names, as.is = !stringsAsFactors, na.strings = "NA",
                             colClasses = NA, nrows = -1, skip = 0, check.names = TRUE,
                             fill = NULL, strip.white = FALSE, blank.lines.skip = TRUE,
                             comment.char = "#", allowEscapes = FALSE, flush = FALSE,
                             stringsAsFactors = FALSE, fileEncoding = "",
                             encoding = "unknown", text, skipNul = FALSE, dim = c(10L,5L)){
  x <- read.table(
    file, header = FALSE, sep = "", quote = "\"'", dec = ".",
    numerals = c("allow.loss", "warn.loss", "no.loss"), row.names,
    col.names, as.is = !stringsAsFactors, na.strings = "NA",
    colClasses = NA, nrows = -1, skip = 0, check.names = TRUE,
    fill = NULL, strip.white = FALSE, blank.lines.skip = TRUE,
    comment.char = "#", allowEscapes = FALSE, flush = FALSE,
    stringsAsFactors = FALSE, fileEncoding = "",
    encoding = "unknown", text, skipNul = FALSE, dim = c(10L,5L), ...)
  if(multiply(dim) > 0) print(x, dim)
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

