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
#' # basic example: pop off the last 2 values from vector
#' c(0,3,"A","Apple", TRUE) #before
#' vector_pop(c(0,3,"A","Apple", TRUE)) #after 1 pop
#' vector_pop(c(0,3,"A","Apple", TRUE), n=3) #after 3 pop
#'
#' # using objects
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
#' #task: remove 4 elements from the end of a vector and return both the removed content and remaining
#' v_f_num <- paste0(number(20),c("TI")) #simulate 20 numbers and add TI suffix
#' v_f_num #show simulated numbers
#' vector_pop(v_f_num, n = 4, ret = TRUE) #get the modified vector
#' vector_pop(v_f_num, n = 4, ret = "removed") #get the content removed
#'
#' #task: remove specific items from vector
#' #note that this aspect of the functionality ignores the 'n' argument
#' v_f_num_2 <- paste0(number(6, seed = 33),c("AB")) #simulate 6 numbers using seed and add AB suffix
#' v_f_num_2 #show numbers
#' vector_pop(v_f_num_2, el = c("403211378AB")) #remove 1 specific entries
#' v_f_num_2 #show results
#' vector_pop(v_f_num_2, el = c("803690460AB","66592309AB")) #remove 2 specific entries
#' v_f_num_2 #show results
#'
#' @export
#'
vector_pop <- function(., n = 1, el = NULL, ret = FALSE){
  .. <- substitute(.)
  if (typeof(..) == "language"){
    return(.[1:(length(.)-n)])
  }
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  init(val,vali,value = get(as.character(..), envir = parent.frame()))

  if(length(val) > 1){
    if (n > length(val))
      stop(paste0("Value of n must not be greater than length of vector content"))
    if(all(not.empty(el))) val <- val[val %nin% el]
    else val <- val[1:(length(val)-n)]
  }else{
    val1 <- strsplit(val,"")[[1]]
    if(not.empty(el)) val <- val1[val1 %nin% el]
    else val <- paste(val1[1:(length(val1)-n)], collapse = "")
  }

  if(ret == FALSE){
    # if return is false, then set the changed vector to the first
    assign(as.character(..),val, envir = parent.frame())
  }else{
    # if return is set to TRUE, return changed vector
    if(ret == TRUE){
      return(val)
    }
    # if return is set to "removed", return removed elements
    if(ret == "removed"){
      return(setdiff(vali,val))
    }
  }
}
