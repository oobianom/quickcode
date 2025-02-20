#' Calculate the Mode of a Numeric or Character Vector
#'
#' This function calculates the mode (most frequently occurring value(s)) of a numeric or character vector.
#'
#' @param x A numeric or character vector for which the mode is to be calculated.
#' @return The mode(s) of the input vector. If multiple values have the same highest frequency, all modes are returned.
#'         Returns `NA` if the input vector is empty.
#' @examples
#' # Example with a numeric vector
#' numeric_vector <- c(1, 2, 2, 3, 3, 3, 4, 5)
#' mode(numeric_vector)
#'
#' # Example with a character vector
#' character_vector <- c("apple", "banana", "apple", "orange", "banana", "banana")
#' mode(character_vector)
#'
#' @export
mode <- function(x) {
  # Check for empty input
  if (length(x) == 0) {
    return(NA)
  }

  # Calculate frequency of each element
  freq_table <- table(x)

  # Find the maximum frequency
  max_freq <- max(freq_table)

  # Return the element(s) with the highest frequency
  mode_values <- names(freq_table[freq_table == max_freq])

  # If numeric, convert to numeric type
  if (is.numeric(x)) {
    mode_values <- as.numeric(mode_values)
  }

  return(mode_values)
}



