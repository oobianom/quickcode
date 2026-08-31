#' Switch the index of two rows in a data set
#'
#' Allows the user to choose precisely which two rows they want to swap places,
#' while optionally preventing some columns from being altered in the process.
#' Excluded columns within the rows act as anchors that are immune from the switching operation
#' on the selected rows.
#'
#' @param data dataset object
#' @param row1 numeric. the first row number
#' @param row2 numeric. the second row number
#' @param keep.cols numeric or character. column number or name to keep
#' @examples
#'
#' # Example using mtcars
#' data100 <- mtcars[1:7,]
#'
#' head(data100) # preview overall data
#'
#' # task 1: basic result of switching rows 5 and 6
#' head(switch_rows(data100, 5, 6))
#'
#' # task 2: switch rows, but retain some columns
#' data100[5:6,2:10] # preview the portion that is to be changed
#'
#' # lets switch 2 rows, but keep content of columns 7, 8, 9 10 within the changed rows
#' res1 <- switch_rows(data100, row1 = 5, row2 = 6, keep.cols = 7:10) # use column numbers
#' res1[5:6,] # check result, pay attention to columns 9 and 10 as well
#' res2 <- switch_rows(data100,
#' row1 = 5,
#' row2 = 6,
#' keep.cols = c("disp","cyl")) # use column names
#' res2[5:6,] # check result, pay attention to columns "disp","cyl" as well
#'
#' @export
switch_rows <- function(data, row1, row2, keep.cols = NULL) {
  # check
  stopifnot(row1 != row2)

  # update row names
  rownames(data)[c(row1, row2)] <- rownames(data)[c(row2, row1)]
  .x2 <- data[row2, ]

  # account for keep.cols
  if (not.null(keep.cols)) {
    if (inherits(keep.cols,"character"))
      keep.cols <- which(names(data) %in% keep.cols)
    .x2[, keep.cols] <- data[row1, keep.cols]
    data[row1, keep.cols] <- data[row2, keep.cols]
  }
  # switch rows
  data[row2, ] <- data[row1, ]
  data[row1, ] <- .x2

  # return output
  data
}


#' Switch the index of two columns in a data set
#'
#' Allows the user to choose precisely which two columns they want to swap places,
#' while optionally preventing some rows within the columns from being altered in the process.
#' Excluded rows within the columns act as anchors that are immune from the switching operation
#' on the selected columns.
#'
#' @param data dataset object
#' @param col1 numeric or character the first column name or number
#' @param col2 numeric or character the second column name or number
#' @param keep.rows numeric. row number to keep
#' @examples
#'
#' # Example using mtcars
#' data101 <- mtcars[1:7,]
#'
#' head(data101) # preview overall data
#'
#' # task 1: basic result of switching columns 5 and 6
#' head(switch_cols(data101, 5, 6))
#'
#' # task 1: basic result of switching columns number 5 and name "gear"
#' head(switch_cols(data101, 5, "gear"))
#'
#' # task 1: basic result of switching columns "qsec" and "carb"
#' head(switch_cols(data101, "qsec", "carb"))
#'
#'
#' # task 2: switch columns, but retain some rows with the switched columns
#'
#'
#' # lets exchange some columns, but keep content of row 4, 5 intact
#' data101[1:6,4:7] # preview the portion that is to be changed
#' res1 <- switch_cols(data101, col1 = 5, col2 = 6, keep.rows = 4:5) # use column numbers
#' res1[1:6,4:7] # check result, pay attention to rows 4, 5 of columns 5, 6 as well
#'
#' data101[1:6,6:11] # preview the portion that is to be changed
#' res2 <- switch_cols(data101,
#' col1 = "qsec",
#' col2 = "carb",
#' keep.rows = c(1,2,3)) # keep 1, 2, 3
#' res2[1:6,6:11] # check result
#'
#' @export
switch_cols <- function(data, col1, col2, keep.rows = NULL) {
  # check
  stopifnot(col1 != col2, not.inherits(keep.rows,"character"))

  # update column names
  if (inherits(col1,"character")) col1 <- which(names(data) %in% col1)
  if (inherits(col2,"character")) col2 <- which(names(data) %in% col2)

  colnames(data)[c(col1, col2)] <- colnames(data)[c(col2, col1)]

  .x2 <- data[,col2, drop=FALSE]

  # account for keep.rows
  if (not.null(keep.rows)) {
    .x2[keep.rows, ] <- data[keep.rows,col1]
    data[keep.rows,col1] <- data[keep.rows,col2]
  }
  # switch columns
  data[, col2] <- data[, col1, drop=FALSE]
  data[, col1] <- .x2

  # return output
  data
}











