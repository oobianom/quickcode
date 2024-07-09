#' Sort vector by length of its content
#'
#' Sort the length of the content of a vector
#'
#' @param vec a vector
#' @param asc TRUE or FALSE whether to sort by ascending or descending order
#' @note
#' This function removes all NAs prior to sorting the vector
#' @return vector of items sorted by length
#'
#' @examples
#' # sort by length of content
#' x = c("acs","tt","jdssr","h","grab")
#' sort_length(vec = x) # ascending order of length
#' sort_length(vec = x, asc = FALSE) # descending order of length
#'
#'
#' @export
#'

sort_length <- function(vec, asc = TRUE) {
  # Remove NA values
  vec <- vec[not.na(vec)]

  # Compute lengths of elements
  lengths <- nchar(vec)

  # Sort indices based on lengths
  # Return sorted vector
  vec[order(lengths, decreasing = !asc)]
}
