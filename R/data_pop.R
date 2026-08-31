#' Remove last n rows or column or specified elements from a data frame like array_pop in PHP
#'
#' Shorthand to remove elements from a data frame and save as the same name
#'
#' @param . parent data
#' @param n number of elements to remove
#' @param which whether to remove from row or from column
#' @param ret TRUE or FALSE. whether to return value instead of setting it to the parent data
#' @return data with elements removed
#' @examples
#' #basic example: pop off 1 row from sample data
#' data.frame(ID=1:10,N=number(10)) #before
#' data_pop(data.frame(ID=1:10,N=number(10))) #after
#'
#' #using data objects
#' data.01 <- mtcars[1:7,]
#'
#' #task: remove 1 element from the end of the data and set it to the data name
#' data.01 #data.01 data before pop
#' data_pop(data.01) #does not return anything
#' data.01 #data.01 data updated after pop
#'
#' #task: remove 3 columns from the end of the data and set it to the data name
#' data.01 #data.01 data before pop
#' data_pop(data.01, n = 3, which = "cols") #does not return anything, but updates data
#' data.01 #data.01 data updated after pop
#'
#' #task: remove 5 elements from the end, but do not set it to the data name
#' data.01 #data.01 data before pop
#' data_pop(data.01,5, ret = TRUE) #return modified data
#' data.01 #data.01 data remains the same after pop
#'
#'
#' @export
#'
data_pop <- function(., n = 1, which = c("rows", "cols"), ret = FALSE) {
  .. <- substitute(.)
  wh <- match.arg(which)
  if (typeof(..) == "language"){
    switch(wh,
           "rows" = {
             d05 <- .[1:(nrow(.)-n),]
           },
           "cols" = {
             d05 <- .[,1:(ncol(.)-n)]
           }
    )
    return(d05)
  }
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
