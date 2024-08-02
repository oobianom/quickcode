#' Wrap code to save space
#'
#' @export

c.ode <- function(...){
  for(i in 1:...length()){
    eval(parse(text=...elt()[i]))
  }
}
