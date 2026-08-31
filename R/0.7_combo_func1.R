# Functions that generally go together often

#' Split a string of numbers and return as numeric vector
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
#' @description
#' The purpose of this function is combine the functionality of
#' \strong{strsplit}, \strong{unlist} and \strong{as.numeric}, which are often used together.\cr\cr
#'
#' @examples
#' # Example 1
#' # string of numbers with separator " "
#' num.01 = "5 3 2 3 5 2 33 23 5 32 432 42 23 554"
#'
#' # split a string of numbers and return as numeric
#' strsplit.num(num.01, split = " ")
#'
#'
#' # Example 2
#' # string of numbers with separator "|||"
#' num.02 = "0|||1|||4|||43|||6|||8|||00||| 1||| 0 1||| T |||F|||TRUE |||f"
#'
#' # split a string of numbers and return as numeric
#' strsplit.num(num.02, split = "[|||]")
#'
#' @export

strsplit.num <-
  function(x,
           split,
           fixed = FALSE,
           perl = FALSE,
           useBytes = FALSE) {
    as.numeric(unlist(strsplit(x, split, fixed, perl, useBytes)))
  }



#' Split a string of values and return as boolean vector
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
#' @description
#' The purpose of this function is combine the functionality of
#' \strong{strsplit}, \strong{unlist} and \strong{as.logical}, which are often used together.\cr\cr
#'
#' @examples
#' # string of numbers
#' num.01 = "0 1 0 0 1 0 1 T F TRUE FALSE t f"
#'
#' # split a string of numbers and return as boolean 1/0
#' strsplit.bool(num.01, split = " ", type = 3)
#'
#' # split a string of numbers and return as boolean TRUE/FALSE
#' strsplit.bool(num.01, split = " ", type = 2)
#'
#' # split a string of numbers and return as boolean Yes/No
#' strsplit.bool(num.01, split = " ", type = 1)
#'
#'
#' # string of numbers
#' num.02 = "0abc1abc0abc0abc1abc0abc1abcTabcFabcTRUEabcFALSEabcf"
#'
#' # split a string of numbers and return as boolean 1/0
#' strsplit.bool(num.02, split = "abc", type = 3)
#'
#' # split a string of numbers and return as boolean TRUE/FALSE
#' strsplit.bool(num.02, split = "abc", type = 2)
#'
#' # split a string of numbers and return as boolean Yes/No
#' strsplit.bool(num.02, split = "abc", type = 1)
#'
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




#' Read a CSV and preview first X rows and columns
#'
#' Read a dataset of type csv and show x rows and y columns with
#' one function call
#'
#' @param dim dimension of CSV content to show
#' @inheritParams utils::read.csv
#' @importFrom utils read.csv
#' @return read csv content and a print out of the data head
#'
#' @description
#' The purpose of this function is combine the functionality of
#' \strong{read.csv} and \strong{print}, which are often used together.\cr\cr
#' The purpose of this function is to read data from a file into a variable and
#' simultaneously display a preview of the data, showing either the first few rows or
#' columns based on the user's specification. It is important to emphasize that the
#' function expects the user to assign the result of the read operation to a variable
#' in order to achieve its intended purpose. eg. Use \strong{var1 = read.csv.print(file1)} instead of
#' \strong{read.csv.print(file1)}
#'
#'
#' @examples
#' \dontrun{
#' # Example: read a csv file and print the first 10 lines
#' # declare file
#' new.file <- "test.csv"
#'
#' # read file and preview default
#' dth3 <- read.csv.print(file = new.file)
#'
#' # read file and preview 10 rows and all columns
#' dth1 <- read.csv.print(file = new.file, dim = 10)
#'
#' # read file and preview 10 rows and 5 columns
#' dth2 <- read.csv.print(file = new.file, dim = c(10,5))
#' }
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
      if (prod(dim) > 0) {
        prtr(x[1:dim[1],1:dim[2]])
      }
    } else {
      prtr(x[1:dim[1],])
    }
    x
  }

#' Read in a table and show first X rows and columns
#'
#' Read a dataset of type table and show x rows and y columns
#'
#' @param dim dimension of table content to show
#' @inheritParams utils::read.table
#' @importFrom utils read.table
#' @return read table content and a print out of the data head
#'
#' @description
#' The purpose of this function is combine the functionality of
#' \strong{read.table} and \strong{print}, which are often used together.\cr\cr
#' The purpose of this function is to read table from a file into a variable and
#' simultaneously display a preview of the data, showing either the first few rows or
#' columns based on the user's specification. It is important to emphasize that the
#' function expects the user to assign the result of the read operation to a variable
#' in order to achieve its intended purpose. eg. Use \strong{var1 = read.table.print(file1)} instead of
#' \strong{read.table.print(file1)}
#'
#'
#' @examples
#' \dontrun{
#' # Example: read a table file and print the first 10 lines
#' # declare file
#' new.file <- "test.csv"
#'
#' # read file and preview default
#' dth3 <- read.table.print(file = new.file, sep=",",quote = "\"",dec = ".",
#' fill = TRUE, comment.char = "", header = TRUE)
#'
#' # read file and preview 10 rows and all columns
#' dth1 <- read.table.print(file = new.file, sep=",",quote = "\"",dec = ".",
#' fill = TRUE, comment.char = "", header = TRUE, dim = 10)
#'
#' # read file and preview 10 rows and 5 columns
#' dth2 <- read.table.print(file = new.file, sep=",",quote = "\"",dec = ".",
#' fill = TRUE, comment.char = "", header = TRUE, dim = c(10,5))
#' }
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
      if (prod(dim) > 0) {
        prtr(x[1:dim[1],1:dim[2]])
      }
    } else {
      if(dim[1] != 0) prtr(x[1:dim[1],])
    }

    x
  }



