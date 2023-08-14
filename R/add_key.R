#' Add index  keys to a vector
#'
#' Index a vector and convert to a list of objects
#'
#' @param vector vector to transform
#' @return a transformed list containing keys along with vector values
#' @examples
#' #ex1 simple conversion of a vector
#' rti2 <- c("rpkg","obinna", "obianom")
#' add_key(rti2)
#' rti2
#'
#' #ex2 add keys to a vector content for use in downstream processes
#' ver1 <- c("Test 1","Test 2","Test 3")
#' add_key(ver1)
#'
#' for(i in ver1){
#'   message(sprintf("%s is the key for this %s", i$key, i$value))
#' }
#'
#' @export
#'
add_key <- function(vector){
  . = list()
  iky = 1
  for(i in vector){
    .[[length(.)+1]] <- list(key = iky, value = i)
    inc(iky)
  }
  #resave to vector name
  .. <- substitute(vector)
  assign(as.character(..), ., envir = parent.frame())
}
