#' Initialize new variables and objects
#'
#' Shorthand to initialize one or more objects
#'
#' @param ... variable names to initialize
#' @param value value to initialize them to
#' @return initialized objects set to the value specified
#'
#' @examples
#' init(t,u,v)
#' message(t) # t = NULL
#' message(u) # u = NULL
#' message(v) # v = NULL
#' init(j,k,m,value = 7)
#' message(j) # j = 7
#' message(k) # k = 7
#' message(m) # m = 7
#'
#' @export
#'
init <- function(...,value = NULL){
  .v <- as.list(substitute(args(...))[-1L])
  for(i in .v)
    assign(as.character(i), value , envir = parent.frame())
}
