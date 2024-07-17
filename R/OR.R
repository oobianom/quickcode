#' Alternative return for statements
#'
#' Return alternative if the value of expression is empty or NA or NULL
#'
#' @param test an object to return
#' @param alternative alternative object to return
#'
#' @export
or <- function(test,alternative){

  res <- test
  if(!length(res)) return(alternative)
  if(is.null(res)|is.na(res) | !not.empty(res)) return(alternative)
  res

}
