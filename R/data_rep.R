#' Duplicate a data rows or columns X times
#'
#' Add a data to itself X times by rows or columns
#'
#' @param . data frame variable
#' @param n multiples of duplicate
#' @param which where to append the duplicated data e.g. rows or cols
#' @return the duplicated dataset store to a variable with the name of the first
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
#' #add p1 to p1 twice by row, and resave as p1
#' data_push(p1,n=2,"rows")
#' p1 #p1 has been updated
#'
#' #add p2 to p2 5 times by cols, and resave as p1
#' data_push(p1,n=5,"cols")
#' p1 #p1 has been updated
#'
#' # declare a new data frame called p3
#' p3 <- data.frame(Hindex=number(20),Rindex=number(20,seed=20))
#'
#' # add p3 to p3 4 times as column, and resave as p3
#' data_push(p3,n=4,"cols")
#' p3 # p3 has been updated
#' @export
#'
data_rep <- function(., n, which = c("rows", "cols")) {
  which <- match.arg(which)
  .. <- substitute(.)
  stopifnot(n>1)

  if (typeof(..) != "symbol"){
    message("Did you forget to declare the variable first? See package examples.??quickcode::data_rep")
    stop(paste0(.., " must be an object. "))
  }

  data <- as.data.frame(get(as.character(..), envir = parent.frame()))

  switch(which,
         "rows" = {
           data <- do.call(rbind, lapply(1:n,function(i)data))
         },
         "cols" = {
           data <- do.call(cbind, lapply(1:n,function(i)data))
         }
  )
  assign(as.character(..), data, envir = parent.frame())
}
