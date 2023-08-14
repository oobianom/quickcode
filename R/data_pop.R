#' Remove last n rows or column or specified elements from a data frame like array_pop in PHP
#'
#' Shorthand to remove elements from a data frame and save as the same name
#'
#' @param . parent vector
#' @param n number of elements to remove
#' @param el vector to remove
#' @param ret TRUE or FALSE. whether to return value instead of setting it to the parent vector
#' @return vector with elements removed
#' @examples
#' data.01 <- mtcars
#'
#' #task: remove 1 element from the end of the vector and set it to the vector name
#' data.01 #data.01 data before pop
#' data_pop(data.01) #does not return anything
#' data.01 #data.01 data updated after pop
#'
#' #task: remove 5 elements from the end, but do not set it to the vector name
#' data.01 #data.01 vector before pop
#' data_pop(data.01,5, ret = TRUE) #return modified vector
#' data.01 #data.01 vector remains the same after pop
#'
#' @export
#'
data_pop <- function(., n = 1, which = c("rows", "cols"), ret = FALSE) {
  .. <- substitute(.)
  wh <- match.arg(which)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  data <- as.data.frame(get(as.character(..), envir = parent.frame()))
  if(nrow(data) & ncol(data)){
    switch(wh,
           "rows" = {
             data <- data[1:(nrow(data)-n),]
           },
           "cols" = {
             data <- data[,1:(ncol(data)-n)]
           }
    )
  }
  if(!ret) assign(as.character(..), data, envir = parent.frame())
  else data
}
