#' Set a variable only once
#'
#' Facilitates the one-time setting of a variable in R, ensuring its immutability thereafter.
#'
#' @param . variable to set
#' @param val the value to set for the variable
#'
#' @return the variable set to the new variable, along with a class of once added to the output
#'
#' @details
#' With this function, users can establish the change to the initial value of a variable,
#' and it guarantees that any subsequent attempts to modify the variable are ignored.
#' This feature ensures that the variable remains constant and immutable once it has been set,
#' preventing unintentional changes and promoting code stability. This function simplifies the process
#' of managing immutable variables in R, providing a reliable mechanism for enforcing
#' consistency in data throughout the course of a program or script.
#'
#' @examples
#' # set the value of vector_x1, vector_y1, vector_z1
#' init(vector_x1, vector_y1, vector_z1, value = 85)
#'
#' # view the initial values of the variables
#' vector_x1
#' vector_y1
#' vector_z1
#'
#' # task 1: change the value vector_x1 and prevent further changes
#' vector_x1 # check value of unchanged
#' vector_x1 * 0.56 # check value when x 0.56
#'
#' setOnce(vector_x1, val = 400) # set vector_x1
#' vector_x1 # check value
#' vector_x1 * 0.56 # check value when x 0.56
#'
#' setOnce(vector_x1, val = 400) # set vector_x1 AGAIN, should not change
#' vector_x1 # check value
#' vector_x1 * 0.56 # check value when x 0.56
#'
#' # task 2: In for loop, change vector_y1 and use later
#' vector_y1 # check value of unchanged
#'
#' for(i in 1:20){
#' setOnce(vector_y1,as.numeric(Sys.time()))
#' # now let's see the difference between vector_y1
#' # and the current time as it changes
#' message(as.numeric(Sys.time()) - vector_y1)
#' }
#'
#' # task 3: In for lapply, change vector_y1 and use later
#' vector_y1 # check value of unchanged
#'
#' lapply(1:20, function(i){
#' setOnce(vector_y1,as.numeric(Sys.time()))
#' # now let's see the difference between vector_y1
#' # and the current time as it changes
#' message(as.numeric(Sys.time()) - vector_y1)
#' })
#'
#'
#' @export

setOnce <- function(., val = 1L) {
  if(!inherits(.,"once")){
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  res <- val
  class(res) <- c('once','list')
  assign(as.character(..), res, envir = .GlobalEnv)
  }
}
