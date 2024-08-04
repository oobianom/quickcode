#' Simple Function chaining
#'
#' Chain multiple function to a call
#'
#' @rdname simplechaining
#' @param obj data object to apply function
#' @param funcs function chains to apply to the data object
#' @param sep separated for funcs argument values
#' @return the result of applying the chained functions to the data object
#' @note
#' chain_sep allows the user to preset the separator for the function chaining \cr\cr
#' e.g. you can call the function to set sep = "__" before using the %c% call to parse the chained function "__unique__sum"
#' @examples
#' #use defult sep ".."
#' 1:3%c%unique.length
#' sample(1:1000,10,replace=TRUE) %c%unique..length
#' sample(1:10,10,replace=TRUE) %c%unique..cumsum
#'
#' # set sep before function chaining
#' chain_sep("__")
#' sample(1:10,10,replace=TRUE) %c%unique__cumsum
#' sample(1:10,10,replace=TRUE) %c%unique__cumsum__length
#' @export

`%c%` <- function(obj, funcs){
  sep. <- options()$quickcode_chain_sep %or% "\\.\\."
  .pF <- as.character(substitute(funcs))
  .pF2 <- strsplit(.pF,sep.)[[1]]
  print(.pF2)
  lapply(.pF2, function(l) eval(parse(text = paste0("obj <<-obj %>% ",l))))
  obj
}


#' @export
#' @rdname simplechaining
chain_sep <- function(sep = "\\.\\."){
  options(quickcode_chain_sep = sep)
}
