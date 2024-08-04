#' Wrap code to save space
#' @examples
#' c.ode({
#' y = 7
#' m = 9
#' },{
#'   m= y +9
#' })
#'
#' @export

c.ode <- function(...){
  for(i in 1:...length()){
   eval(parse(text=...elt(i)))
  }
}



