#' Add data to another data like array_push in PHP
#'
#' Shorthand to add data to a dataset and save as the same name
#'
#' @param . first data set
#' @param add data set to add
#' @param which where to append the new data e.g. rows or cols
#' @return the combined dataset store to a variable with the name of the first
#' @examples
#' # initialize p1 and p2
#' init(p1,p2)
#' p1
#' p2
#'
#' # declare p1 and p2 as data frame
#' p1 <- data.frame(PK=1:10,ID2=1:10)
#' p2 <- data.frame(PK=11:20,ID2=21:30)
#'
#' p1
#' p2
#'
#' #add p1 to p2 by row, and resave as p1
#' data_push(p1,p2,"rows")
#' # p2 # p2 remains the same
#' p1 #p1 has been updated
#'
#' # declare a new data frame called p3
#' p3 <- data.frame(Hindex=number(20),Rindex=number(20,seed=20))
#'
#' # add p3 to p1 as column, and resave as p1
#' data_push(p1,p3,"cols")
#' p1 # p1 has been updated
#' @export
#'
data_push <- function(., add, which = c("rows", "cols")) {
  which <- match.arg(which)
  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  data <- as.data.frame(get(as.character(..), envir = parent.frame()))
  add <- as.data.frame(add)
  switch(which,
         "rows" = {
           data <- rbind(data,add)
         },
         "cols" = {
           data <- cbind(data,add)
         }
  )
  assign(as.character(..), data, envir = parent.frame())
}
