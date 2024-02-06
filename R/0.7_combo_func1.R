# Combinatory functions
#' Split a string of numbers and return numeric
#'
#' Given a sting, split by a separator into numbers
#'
#' @inherit  base strsplit
#' @export

strsplit.num <- function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE){
  as.numeric(unlist(strsplit(x, split, fixed, perl, useBytes)))
}
