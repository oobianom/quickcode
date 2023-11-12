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
