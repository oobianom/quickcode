#' Count the number of decimal places
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


saml <- function(...){
  print(...length())
  k=match.call()[-1L]
  print(names(k))

  for(y in 1:3){
  r= ...elt(y)
  print(r)
}

  k
}
saml(y=8,k=9,list(1:5))
...el

