# Future release: function in development

#' Flatten and combine strings
#'
#' Flatten vectors and combine together
#'
#' @param j string or vector
#' @param h second string or vector
#' @return combined strings
#' @note this is simply a way to further simplify paste and paste0 into a function
#' @examples
#' "obi" %~% "is"
#' "you" %~% "are" ~ "my" %~% "friend"
#' c("none","is","better") %~% "for" %~% "now"
#'
#'

`%~%` <- function(j, h, sep = " ") {

  if (length(j) > 1) j <- paste(j, sep = sep)
  if (length(h) > 1) h <- paste(h, sep = sep)

  if( (is.character(j) & is.character(h)) | (typeof(h)!=typeof(j)) ){
    paste0(j, sep, h)
  } else {
    stop("Must be same type")
  }
}
