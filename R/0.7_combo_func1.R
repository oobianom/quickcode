# Combinatory functions

#' Split a string of numbers and return numeric
#'
#' Given a sting, split by a separator into numbers
#'
#' @inheritParams base  strsplit
#' @return numeric values based on split string
#' @export

strsplit.num <- function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE){
  as.numeric(unlist(strsplit(x, split, fixed, perl, useBytes)))
}



#' Split a string of values and return as boolean
#'
#' Given a sting, split by a separator into boolean
#'
#' @inheritParams base strsplit
#' @param type type of return, see the as.boolean function for more info
#' @return boolean values based on split string
#' @export

strsplit.bool <- function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE,type = 2){
  as.boolean(unlist(strsplit(x, split, fixed, perl, useBytes)),type = type)
}
