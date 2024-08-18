#' Function to calculate the percentage of matching between two strings
#' @param string1 first string
#' @param string2 second string
#' @examples
#' Example usage
#' string1 <- "Hello World"
#' string2 <- "helo world"
#'
#' match_percent <- percent_match(string1, string2)
#' message("Percentage of matching: ", match_percent)
#'
#' @export
#'

percent_match <- function(string1, string2) {
  # Convert strings to lowercase to make the comparison case-insensitive
  string1 <- tolower(string1)
  string2 <- tolower(string2)

  # Calculate the Levenshtein distance between the two strings
  dist <- adist(string1, string2)

  # Determine the maximum possible distance (i.e., the length of the longer string)
  max_len <- max(nchar(string1), nchar(string2))

  # Calculate the percentage match
  percent <- (1 - dist / max_len) * 100

  # Return the percentage match
  return(round(percent, 2))
}

