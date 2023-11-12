#' Variation of seq
#'
#' increment a number till you get count
#'
#' @examples
#' #ex1
#' seq3(2,10,n=5)
#'
#' @export
#'
seq3 <- function(start,increment,count = 10) seq(start,start*count,increment)


