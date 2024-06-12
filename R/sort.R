#' Sort vector by length of its content
#'
#' Sort the length of the content of a vector
#'
#' @param vec a vector
#' @param asc TRUE or FALSE whether to sort by ascending or descending order
#' @note
#' This function removes all NAs prior to sorting the vector
#'
#' @examples
#' # sort by length of content
#' x = c("acs","tt","jdssr","h","grab")
#' y = sort.length(vec = x)
#' z = sort.length(vec = x, asc = FALSE)
#'
#'
#' @export
#'

sort.length <- function(vec, asc = TRUE) {
  # Remove NA values
  vec <- vec[not.na(vec)]

  # Compute lengths of elements
  lengths <- nchar(vec)

  # Sort indices based on lengths
  # Return sorted vector
  vec[order(lengths, decreasing = !asc)]
}
