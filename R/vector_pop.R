#' Remove last n elements or specified elements from a vector like array_pop in PHP
#'
#' Shorthand to remove elements from a vector and save as the same name
#'
#' @param . parent vector
#' @param n number of elements to remove
#' @param el vector to remove
#' @param ret TRUE or FALSE. whether to return value instead of setting it to the parent vector
#' @return vector with elements removed
#' @examples
#' num1 <- sample(330:400,10)
#' name1 <- "ObinnaObianomObiObianom"
#'
#' #task: remove 1 element from the end of the vector and set it to the vector name
#' num1 #num1 vector before pop
#' vector_pop(num1) #does not return anything
#' num1 #num1 vector updated after pop
#'
#' #task: remove 5 elements from the end, but do not set it to the vector name
#' num1 #num1 vector before pop
#' vector_pop(num1,5, ret = TRUE) #return modified vector
#' num1 #num1 vector remains the same after pop
#'
#'
#' #task: remove 6 elements from a word, set it back to vector name
#' name1 #name1 before pop
#' vector_pop(name1,6) #does not return anything
#' name1 #name updated after pop
#'
#' #task: remove 3 elements from a word, Do not set it back to vector name
#' name1 #name1 before pop
#' vector_pop(name1,3, ret = TRUE) #returns modified name1
#' name1 #name1 not updated after pop
#'
#'
#' @export
#'
vector_pop <- function(., n = 1, el = NULL, ret = c(FALSE,TRUE,"removed")){
  ret <- match.arg(ret)
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  init(val,vali,value = get(as.character(..), envir = parent.frame()))

  if(length(val) > 1){
    if (n > length(val))
      stop(paste0("Value of n must not be greater than length of vector content"))
    if(not.empty(el)) val <- val[val != el]
    else val <- val[1:(length(val)-n)]
  }else{
    val1 <- strsplit(val,"")[[1]]
    if(not.empty(el)) val <- val1[val1 != el]
    else val <- paste(val1[1:(length(val1)-n)], collapse = "")
  }

  if(ret == FALSE){
    # if return is false, then set the changed vector to the first
    assign(as.character(..),val, envir = parent.frame())
  }else{
    # if return is set to TRUE, return changed vector
    if(ret == TRUE){
      val
    }

    # if return is set to "removed", return removed elements
    if(ret == "removed"){
      setdiff(val,vali)
    }
  }
}
