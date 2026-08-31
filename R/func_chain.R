#' simple function chaining routine
#'
#' chain multiple function to a call
#'
#' @rdname simplechaining
#' @param obj data object to apply function
#' @param funcs function chains to apply to the data object
#' @param sep separator for functions argument values
#' @return the result of applying the chained functions to the data object
#' @note
#' chain_sep allows the user to preset the separator for the function chaining \cr\cr
#' e.g. you can call the function to set sep = "__" before using the %.% call to parse the chained function "__unique__sum"
#' @examples
#' #use default sep ".."
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





#' Combine specific functions as store as one function
#'
#' Use many functions in one call using predeclared calls
#'
#' @param ... functions to include. see example for how to use
#' @param otherargs other arguments for use in each function
#' @rdname simplechaining2
#' @return returns a function that combines multiple function
#' @export
#' @examples
#'
#' # Example 1 with base functions
#' combf1 <- chain_func(unique, cumsum, print)
#' result <- combf1(sample(1:5,10,replace = TRUE))
#' #or
#' u = sample(1:5,10,replace = TRUE)
#' result <- combf1(u)
#'
#' # Example 2 with base functions with arguments
#' combf2 <- chain_func(unique, print, otherargs = list(.= c(FALSE),.=c(2)))
#' result <- combf2(sample(1:3,10,replace = TRUE))
#'
#' # Example 3 with custom functions
#' r = function(a,b,c){
#'   if(!missing(a))print(a)
#'   if(!missing(b))print(b)
#'   if(!missing(c))print(c)
#'   return(a)
#' }
#'
#' r2 = function(a,b,c){
#'   if(!missing(a))message(a)
#'   if(!missing(b))message(b)
#'   if(!missing(c))message(c)
#'   return(a)
#' }
#'
#' combf3 <- chain_func(r,r2, otherargs =list(.=c("apple","cat"),.=c("rice")))
#' res <- combf3(head(mtcars))

chain_func <- function(...,otherargs = list()) {
  # Store the functions in a list
  functions <- list(...)
  # Create a function to apply the chained functions
  return(function(x) {
    result <- x
    for (func in indexed(functions)) {
      list1 = list(result)
      for (le in unlist(otherargs[func$key])) list1<- c(list1,list(le))
      result <- do.call(func$value,list1)
    }
    return(result)
  })
}

