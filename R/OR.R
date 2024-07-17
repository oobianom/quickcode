#' @export
or <- function(test,alternative){

  res <- test
  if(!length(res)) return(alternative)
  if(is.null(res)|is.na(res) | !not.empty(res)) return(alternative)
  res

}
