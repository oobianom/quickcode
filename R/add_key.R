#' Add index  keys to a vector or data frame or list or matrix
#'
#' Index a vector or lists and convert to a list of objects
#'
#' @param vector vector or data frame to transform
#' @param key variable name for keys
#' @param value variable name for values
#' @return a transformed list containing keys along with vector values
#' @details
#' This function takes a vector and turns it into a list containing 'key' and 'value' for each vector.
#' This allows the output to be used in loops such as for loops or lapply or other functions to track
#' the index of the list content e.g. 1,2,3...\cr\cr
#'
#' This function also contains a validator to ensure that a vector had not been previously 'keyed',
#' which prevents the user from inadvertently calling the function twice on a vector. Helps especially
#' because the function keys the vector, and sets the new list to the variable name of the original
#' vector.
#'
#' @note
#' add_key - resaves the keys and value pairs to original variable\cr\cr
#' indexed - return the keys and value pairs
#'
#' @section Use case:
#' Efficient for loops and for tracking various steps through a vector contents
#' @rdname addkey
#' @examples
#' # EXAMPLES for add_key()
#'
#'
#' #ex1 simple conversion of a vector
#' rti2 <- c("rpkg","obinna", "obianom")
#' add_key(rti2)
#' rti2
#'
#' #ex2 add keys to a vector content for use in downstream processes
#' ver1 <- c("Test 1","Test 2","Test 3")
#' add_key(ver1)
#'
#' #ex3 use keyed ver1 in for loop
#' for(i in ver1){
#'   message(sprintf("%s is the key for this %s", i$key, i$value))
#' }
#'
#' #ex4 use keyed ver1 in lapply loop
#' xl1 <- lapply(ver1,function(i){
#'   message(sprintf("lapply - %s is the key for this %s", i$key, i$value))
#' })
#'
#'
#' @export
#'
add_key <- function(vector){
  if(inherits(vector,"klist")) stop("Key already added to object.")

  #create list and add keys
  . = list()
  iky = 1
  for(i in vector){
    .[[length(.)+1]] <- list(key = iky, value = i)
    inc(iky)
  }
  #resave to vector name
  .. <- substitute(vector)
  class(.) <- c('klist','list')
  assign(as.character(..), ., envir = parent.frame())
}




#' @rdname addkey
#' @details
#' This function takes a vector and turns it into a list containing 'key' and 'value' for each vector.
#' This allows the output to be used in loops such as for loops or lapply or other functions to track
#' the index of the list content e.g. 1,2,3...\cr\cr
#'
#' This function also contains a validator to ensure that a vector had not been previously 'keyed',
#' which prevents the user from inadvertently calling the function twice on a vector. Helps especially
#' because the function keys the vector, and sets the new list to the variable name of the original
#' vector.
#'
#'
#'
#' @examples
#' # EXAMPLES for indexed()
#'
#' #ex1 simple conversion of a vector
#' rti2 <- c("rpkg","obinna", "obianom")
#' indexed(rti2)
#'
#' #ex2 add keys to a vector content for use in downstream processes
#' ver1 <- c("Test 1","Test 2","Test 3")
#'
#'
#' #ex3 use keyed ver1 in for loop
#' for(i in indexed(ver1)){
#'   message(sprintf("%s is the key for this %s", i$key, i$value))
#' }
#'
#' #ex4 use keyed ver1 in for loop
#' #specify name for key and value
#' for(i in indexed(ver1,k,v)){
#'   message(
#'   sprintf("%s is the new key for this value %s",
#'   i$k, i$v))
#' }
#'
#' #ex5 use keyed ver1 in lapply loop
#' xl1 <- lapply(indexed(ver1),function(i){
#'   message(sprintf("lapply - %s is the key for this %s", i$key, i$value))
#' })
#'
#'
#' @export
#'
indexed <- function(vector,key = key,value = value){
  #create list and add keys
  .. = list()
  count = 1
  for(i in vector){
    num <- length(..)+1
    ..[[num]] <- list()
    ..[[num]][[as.character(substitute(key))]] = count
    ..[[num]][[as.character(substitute(value))]] = i
    inc(count) #increment count
  }
  ..
}
