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
#' #add p1  twice by row, and resave as p1
#' data_rep(p1,n=2,"rows")
#' p1 #p1 has been updated
#'
#'
#' #add p2  3 times by col, and resave as p2
#' data_rep(p2,n=3,"cols")
#' p2 #p2 has been updated
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
