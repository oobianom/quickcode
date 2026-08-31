#' Add elements to a vector like array_push in PHP
#'
#' Shorthand to add elements to a vector and save as the same name
#'
#' @param . first vector
#' @param add vector to add
#' @param unique remove duplicated entries
#' @param rm.na remove NA values
#' @param rm.empty remove empty values
#' @return vector combining fist and second vector, but have name set to the first
#'
#' @section Use case:
#' This function allows the combination of two vectors in one short line of code. It allows specification of further downstream
#' filtering of the resulting vector such as selecting only unique items, removing NA or empty values. It simplifies a code chunk
#' with many lines of code to concatenate and filter various vectors.
#'
#' @details
#' Note that two vectors are required in order to use this function. Also, note that the final result replaces the content of the
#' first vector. This means that the original content of the 'first vector' will no longer exist after this function executes.
#'
#' @examples
#' num1 <- number(10, seed = 45)
#' num2 <-"rpkg.net"
#'
#' num1
#' num2
#'
#' #Task: add num2 to num1 and re-save as num1
#' vector_push(num1,num2)
#' num1 #updated with num2
#' num2 #not updated
#'
#'
#' #Task: concatenate two vectors and remove duplicates
#' vector1 = number(4,seed = 5)
#' vector2 = number(8,seed = 5)
#' vector3 = number(12,seed = 5)
#'
#' vector1 #length is 4
#' vector2 #length is 8
#' vector3 #length is 12
#'
#' # with duplicated
#' vector_push(vector1,vector2, unique = FALSE)
#' vector1 #return modified vector
#' length(vector1) #length is 12 because nothing was removed
#' #duplicates in vector1 is 886905927 100040083 293768998  54080431
#'
#' # without duplicated
#' vector_push(vector2,vector3, unique = TRUE)
#' vector2 #return modified vector
#' length(vector2) #length is 12 instead of 20
#' #Total of 8 duplicated numbers were removed
#'
#'
#' #Task: concatenate two vector and remove NA values
#' vector1 = number(5)
#' vector2 = c(4,NA,5,NA)
#' vector3 = number(5)
#'
#' # with NA
#' vector_push(vector1,vector2, rm.na = FALSE)
#' vector1 #return modified vector
#'
#' # without NA
#' vector_push(vector3,vector2, rm.na = TRUE)
#' vector3 #return modified vector
#'
#'
#' #Task: concatenate two vector and remove empty values
#' vector1 = number(5)
#' vector2 = c(4,'',5,'',NULL,' ')
#' vector3 = number(5)
#'
#' # with empty
#' vector_push(vector1,vector2, rm.empty = FALSE)
#' vector1 #return modified vector
#'
#' # without empty
#' vector_push(vector3,vector2, rm.empty = TRUE)
#' vector3 #return modified vector
#'
#' @export
#'
vector_push <- function(., add, unique = FALSE, rm.na = FALSE, rm.empty = FALSE) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  .res <- c(get(as.character(..), envir = parent.frame()), add)
  if(unique) .res <- unique(.res)
  if(rm.na) .res <- .res[not.na(.res)]
  if(rm.empty) .res <- .res[not.empty(.res)]
  assign(as.character(..), .res , envir = parent.frame())
}

