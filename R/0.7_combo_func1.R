# Functions that generally go together often

#' Split a string of numbers and return numeric
#'
#' Given a sting, split by a separator into numbers
#'
#' @param x character vector, each element of which is to be split. Other inputs, including a factor, will give an error.
#' @param split character vector
#' @param perl logical. Should Perl-compatible regexps be used?
#' @param useBytes 	logical. If TRUE the matching is done byte-by-byte rather than character-by-character, and inputs with marked encodings are not converted.
#' @param fixed logical. If TRUE match split exactly, otherwise use regular expressions. Has priority over perl.
#' @return numeric values based on split string
#'
#' @examples
#' # to be added
#' @export

strsplit.num <-
  function(x,
           split,
           fixed = FALSE,
           perl = FALSE,
           useBytes = FALSE) {
    as.numeric(unlist(strsplit(x, split, fixed, perl, useBytes)))
  }



#' Split a string of values and return as boolean
#'
#' Given a sting, split by a separator into boolean
#'
#' @param x character vector, each element of which is to be split. Other inputs, including a factor, will give an error.
#' @param split character vector
#' @param perl logical. Should Perl-compatible regexps be used?
#' @param useBytes 	logical. If TRUE the matching is done byte-by-byte rather than character-by-character, and inputs with marked encodings are not converted.
#' @param fixed logical. If TRUE match split exactly, otherwise use regular expressions. Has priority over perl.
#' @param type type of return, see the as.boolean function for more info
#' @return boolean values based on split string
#'
#'
#' @examples
#' # to be added
#' @export

strsplit.bool <-
  function(x,
           split,
           fixed = FALSE,
           perl = FALSE,
           useBytes = FALSE,
           type = 2) {
    as.boolean(unlist(strsplit(x, split, fixed, perl, useBytes)), type = type)
  }




#' Read in a CSV and show first X rows and Columns
#'
#' Read a dataset of type csv and show x rows and y columns
#'
#' @param dim dimension of CSV content to show
#' @inheritParams utils::read.csv
#' @importFrom utils read.csv
#' @return read csv content
#'
#' @examples
#' # to be added
#' @export

read.csv.print <-
  function(file,
           header = TRUE,
           sep = ",",
           quote = "\"",
           dec = ".",
           fill = TRUE,
           comment.char = "",
           ...,
           dim = c(10L, 5L)) {
    x <- read.table.print(
      file = file,
      header = header,
      sep = sep,
      quote = quote,
      dec = dec,
      fill = fill,
      comment.char = comment.char,
      dim = 0,
      ...
    )
    if (length(dim) > 1 & dim[1] != 0) {
      if (multiply(dim) > 0) {
        prtr(x[1:dim[1],1:dim[2]])
      }
    } else {
      prtr(x[1:dim[1],])
    }
    x
  }

#' Read in a CSV and show first X rows and Columns
#'
#' Read a dataset of type csv and show x rows and y columns
#'
#' @param dim dimension of table content to show
#' @inheritParams utils::read.table
#' @importFrom utils read.table
#' @return read table content
#'
#' @examples
#' # to be added
#' @export

read.table.print <-
  function(file,
           header = FALSE,
           sep = "",
           quote = "\"'",
           dec = ".",
           numerals = c("allow.loss", "warn.loss", "no.loss"),
           row.names,
           col.names,
           as.is = TRUE,
           na.strings = "NA",
           colClasses = NA,
           nrows = -1,
           skip = 0,
           check.names = TRUE,
           fill = NULL,
           strip.white = FALSE,
           blank.lines.skip = TRUE,
           comment.char = "#",
           allowEscapes = FALSE,
           flush = FALSE,
           stringsAsFactors = FALSE,
           fileEncoding = "",
           encoding = "unknown",
           skipNul = FALSE,
           dim = c(10L, 5L),
           ...) {
    x <- read.table(
      file = file,
      header = header,
      sep = sep,
      quote = quote,
      dec = dec,
      numerals = match.arg(numerals),
      row.names = row.names,
      col.names = col.names,
      as.is = as.is,
      na.strings = na.strings,
      colClasses = colClasses,
      nrows = nrows,
      skip = skip,
      check.names = check.names,
      fill = fill,
      strip.white = strip.white,
      blank.lines.skip = blank.lines.skip,
      comment.char = comment.char,
      allowEscapes = allowEscapes,
      flush = flush,
      stringsAsFactors = stringsAsFactors,
      fileEncoding = fileEncoding,
      encoding = encoding,
      skipNul = skipNul,
      ...
    )
    if (length(dim) > 1 & dim[1] != 0) {
      if (multiply(dim) > 0) {
        prtr(x[1:dim[1],1:dim[2]])
      }
    } else {
      if(dim[1] != 0) prtr(x[1:dim[1],])
    }

    x
  }



