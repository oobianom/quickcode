# Function that generally go together often

#' Split a string of numbers and return numeric
#'
#' Given a sting, split by a separator into numbers
#'
#' @inheritParams base::strsplit
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
  if(length(dim)>1 & dim != 0){ if(multiply(dim) > 0) head(x, dim) }else{
    head(x, n = dim)
  }
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
                             col.names, as.is = TRUE, na.strings = "NA",
                             colClasses = NA, nrows = -1, skip = 0, check.names = TRUE,
                             fill = NULL, strip.white = FALSE, blank.lines.skip = TRUE,
                             comment.char = "#", allowEscapes = FALSE, flush = FALSE,
                             stringsAsFactors = FALSE, fileEncoding = "",
                             encoding = "unknown", skipNul = FALSE, dim = c(10L,5L),...){
  x <- read.table(
    file = file, header = header, sep = sep, quote = quote, dec = dec,
    numerals = match.arg(numerals), row.names = row.names,
    col.names = col.names, as.is = as.is, na.strings = na.strings,
    colClasses = colClasses, nrows = nrows, skip = skip, check.names = check.names,
    fill = fill, strip.white = strip.white, blank.lines.skip = blank.lines.skip,
    comment.char = comment.char, allowEscapes = allowEscapes, flush = flush,
    stringsAsFactors = stringsAsFactors, fileEncoding = fileEncoding,
    encoding = encoding, skipNul = skipNul, ...)
  if(length(dim)>1 & dim != 0){ if(multiply(dim) > 0) head(x, dim) }else{
    head(x, n = dim)
  }

  x
}



#' Multiple a vector of numeric values
#'
#' Mulitple all the content of a vector
#'
#' @param ... the numeric values to multiply
#' @return multiple of all content
#'
#' @examples
#' # multiply 1 number
#' # returns error
#' multiply(0)
#'
#' # vector of numbers
#' numvec <- number(10, max.digits=3)
#' numvec
#'
#' # multiply 2 numbers
#' multiply(numvec[1:2])
#' multiply(numvec[4],numvec[5])
#' multiply(a=4,b=5)
#'
#' # multiply 5 numbers
#' multiply(numvec[1:5])
#' multiply(11,15,12,14,13)
#' multiply(a=4,b=22,c=44,d=9,u=10)
#'
#' @export

multiply <- function(...) {
  args <- unlist(c(as.list(environment()), list(...)))
  stopifnot(length(args) > 1)
  tail(cumprod(args), n = 1)
}


