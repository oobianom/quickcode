Date1to3 <- function(data){
  if (class(data) != "Date") { stop("class(data) is not an object of class Date")
  }
  str = as.character(data)
  yr1 = easyr::left(str, 4)
  mth1 = easyr::mid(str, 6, 2)
  day1 = easyr::right(str, 2)
  x = data.frame(yr1, mth1, day1)
  x
}
Date3to1 <- function(...){

}

#' @export
switch_rows <- function(data,row1,row2,keep){
  .x2 <- data[row2,]
  data[row2,] <- data[row1,]
  data[row1,] <- .x2
  data
}
