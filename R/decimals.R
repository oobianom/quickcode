#' Count the number of decimal places
#'
#' @param num a numeric value
#'
#' @examples
#' #example 1
#' ndecimal(9.000322)
#'
#' #example 2
#' ndecimal(34)
#'
#' #example 3
#' ndecimal(45.)
#'
#' @export

ndecimal<-function(num){
  stopifnot(inherits(num,"numeric")|inherits(num,"integer"))
  unlist(lapply(num,function(x){
    if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
  }))
}
#



