#' Add data to another data like array_push in PHP
#'
#' Shorthand to add data to a dataset and save as the same name
#'
#' @param . first data set
#' @param add data set to add
#' @param which where to append the new data e.g. rows or cols
#' @return the combined dataset store to a variable with the name of the first
#' @examples
#' p1 <- data.frame(PK=1:10,ID2=1:10)
#' p2 <- data.frame(PK=11:20,ID2=21:30)
#' data_push(p1,p2,"rows")
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
