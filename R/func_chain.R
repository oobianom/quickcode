#' simple function chaining routine
#'
#' chain multiple function to a call
#'
#' @rdname simplechaining
#' @param obj data object to apply function
#' @param funcs function chains to apply to the data object
#' @param sep separated for funcs argument values
#' @return the result of applying the chained functions to the data object
#' @note
#' chain_sep allows the user to preset the separator for the function chaining \cr\cr
#' e.g. you can call the function to set sep = "__" before using the %.% call to parse the chained function "__unique__sum"
#' @examples
#' #use defult sep ".."
#' 1:3%.%unique..length
#' sample(1:1000,10,replace=TRUE) %.%unique..length
#' sample(1:10,10,replace=TRUE) %.%unique..cumsum
#'
#' # set sep before function chaining
#' chain_sep("__")
#' sample(1:10,10,replace=TRUE) %.%unique__cumsum
#' sample(1:10,10,replace=TRUE) %.%unique__cumsum__length
#'
#'
#' # set sep before function chaining
#' chain_sep("X")
#' sample(1:10,10,replace=TRUE) %.%uniqueXcumsum
#' @export

`%.%` <- function(obj, funcs){
  sep. <- options()$quickcode_chain_sep %or% "\\.\\."
  .pF <- as.character(substitute(funcs))
  .pF2 <- trimws(strsplit(.pF,sep.)[[1]])

  for(u in .pF2) obj<- do.call(u,list(obj))
  #lapply(.pF2, function(l) eval(parse(text = paste0("obj<<- ",l,"(obj)"))))
  obj
}


#' @export
#' @rdname simplechaining
chain_sep <- function(sep = "\\.\\."){
  options(quickcode_chain_sep = sep)
}
