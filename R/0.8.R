#' @export
date1to3 <- function(data){
  if (class(data) != "Date") { stop("class(data) is not an object of class Date")
  }
  str = as.character(data)
  yr1 = easyrleft(str, 4)
  mth1 = easyrmid(str, 6, 2)
  day1 = easyrright(str, 2)
  data.frame(yr1, mth1, day1)
}
#' @export
date3to1 <- function(dat,sep="-"){
  paste(dat[,1],dat[,2],dat[,3],collapse = sep)
}

#' @export
switch_rows <- function(data,row1,row2,keep){
  .x2 <- data[row2,]
  data[row2,] <- data[row1,]
  data[row1,] <- .x2
  data
}


easyrleft <-
function (string, char)
  substr(string, 1, char)

easyrmid <-
function (string, start, nchars)
  substr(string, start, start + nchars - 1)

easyrright<-
function (string, char)
  substr(string, nchar(string) - (char - 1), nchar(string))




# print all the environment object and sizes and connections to each other
summarize.envObj <- function(){

}

#function execution time

fun.time <- function(...){
  .m <- Sys.time()
  local(...)
  Sys.time() - .m
}
