#' Combine unique() and length()
#'
#' Combine two frequently used function together to return the length of the unique items of a vector
#'
#' @param . object such as vector or names(dataframe)
#' @return length of the unique items in a vector
#'
#' @examples
#' frenchnames1 = c("Léa","Obinna","Bastien","Léa","Obinna", "Hugo", "Amélie","Louis")
#' uniqLen(frenchnames1)
#'
#' @export

uniqLen <- function(.){
  length(unique(.))
}
