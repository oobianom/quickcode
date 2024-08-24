#' Function to calculate the percentage of matching between two strings
#' @param string1 first string
#' @param string2 second string
#' @param case_sensitive if to check case sensitivity
#' @param ignore_whitespace if to ignore whitespace
#' @param frag_size fragment size of string
#' @rdname percentmatch1
#' @return numeric value of the match percent
#' @examples
#' # Example 1: simple match
#' string1 <- "Hello World"
#' string2 <- "helo world"
#'
#' match_percent <- percent_match(string1, string2)
#' message("Percentage of matching: ", match_percent)
#'
#'
#' # Example 2: which date is closest
#' string0 <- "october 12,1898"
#' string1 <- "2018-10-12"
#' string2 <- "1898-10-12"
#' percent_match(string0, string1)
#' percent_match(string0, string2)
#' percent_match(string0, string2, frag_size = 4)
#' percent_match(string1, string2)
#'
#' @export
#'
#' @details
#' Case Sensitivity: \cr
#'
#'   The function can optionally consider or ignore case sensitivity based on the \code{case_sensitive} argument.\cr\cr
#' Whitespace Handling:\cr
#'
#'   With \code{ignore_whitespace} set to TRUE, the function removes all whitespaces before comparison. This can be useful for matching strings that may have inconsistent spacing.\cr\cr
#' Exact Character-by-Character Matching:\cr
#'
#'   The function computes the percentage of matching characters in the same positions.\cr\cr
#' Substring Matching:\cr
#'
#'   The function checks if one string is a substring of the other, awarding a full match if true.\cr\cr
#' Levenshtein Distance:\cr
#'
#'   The function uses Levenshtein distance to calculate the similarity and integrates this into the overall match percentage.\cr\cr
#' Fragment Matching:\cr
#'
#'    - A \code{frag_size} argument is introduced that compares fragments (substrings) of a given size (default is 3) from both strings.\cr
#'  - The function creates unique fragments from each string and compares them to find common fragments.\cr
#'  - The percentage match is calculated based on the ratio of common fragments to the total number of unique fragments.\cr\cr
#' Combining Metrics:\cr
#'
#'   The overall match percentage is computed as the average of exact match, substring match, Levenshtein match, and fragment match percentages.
#'
#'

percent_match <-
  function(string1,
           string2,
           case_sensitive = FALSE,
           ignore_whitespace = TRUE,
           frag_size = 2) {
    # Optional case-insensitive comparison
    if (!case_sensitive) {
      string1 <- tolower(string1)
      string2 <- tolower(string2)
    }

    # Optional whitespace handling
    if (ignore_whitespace) {
      string1 <- gsub("\\s+", "", string1) # Remove all whitespace
      string2 <- gsub("\\s+", "", string2)
    }

    # Exact character-by-character matching
    max_len <- nchar(string1) # max(nchar(string1), nchar(string2))
    if (nchar(string1) == nchar(string2)) {
      char_match <-
        sum(strsplit(string1, NULL)[[1]] == strsplit(string2, NULL)[[1]])
      exact_match_percent <- (char_match / max_len) * 100
    } else {
      exact_match_percent <- 0
    }

    # Substring matching (percentage of one string being a substring of the other)
    if (grepl(string1, string2) || grepl(string2, string1)) {
      substring_match_percent <- 100
    } else {
      substring_match_percent <- 0
    }


    # Levenshtein distance based matching
    lev_dist <- utils::adist(string1, string2)
    levenshtein_match_percent <- (1 - lev_dist / max_len) * 100

    f_m_p <- fragment_match(string1, string2, frag_size)

    # Combine all metrics (weighted average or any other logic)
    # Here, we give equal weight to exact match, Levenshtein distance, and fragment match
    overall_match_percent <-
      (exact_match_percent + levenshtein_match_percent + substring_match_percent + f_m_p) / 4

    # Return a list of different match percentages for deeper insights
    return(
      list(
        exact_match_percent = round(exact_match_percent, 2),
        substring_match_percent = substring_match_percent,
        levenshtein_match_percent = round(levenshtein_match_percent, 2),
        f_m_p = round(f_m_p, 2),
        overall_match_percent = round(overall_match_percent, 2)
      )
    )
  }

# Fragment matching
fragment_match <- function(str1, str2, frag_size) {
  fragments1 <-
    unique(unlist(lapply(1:(nchar(str1) - frag_size + 1), function(i)
      substring(str1, i, i + frag_size - 1))))
  fragments2 <-
    unique(unlist(lapply(1:(nchar(str2) - frag_size + 1), function(i)
      substring(str2, i, i + frag_size - 1))))
  common_fragments <- intersect(fragments1, fragments2)
  f_m_p <-
    (length(common_fragments) / length(union(fragments1, fragments2))) * 100

  return(f_m_p)
}
#' @export
#' @rdname percentmatch1
`%match%` <- function(string1,string2) percent_match(string1,string2)

#' @export
#' @rdname percentmatch1
#' @return match word sounds
#' @examples
#' sound_match("Robert","rupert")
#' sound_match("rupert","Rubin")
#' sound_match("book","oops")
sound_match <- function(string1,string2){
  soundex_m(string1) == soundex_m(string2)
}
soundex_m <- function(name) {
  # Convert to uppercase
  name <- toupper(name)

  # Retain the first letter
  first_letter <- substr(name, 1, 1)

  # Replace letters with corresponding Soundex digits
  name <- gsub("[BFPV]", "1", name)
  name <- gsub("[CGJKQSXZ]", "2", name)
  name <- gsub("[DT]", "3", name)
  name <- gsub("L", "4", name)
  name <- gsub("[MN]", "5", name)
  name <- gsub("R", "6", name)

  # Replace adjacent same digits with a single digit
  name <- gsub("(\\d)\\1+", "\\1", name)

  # Remove vowels (A, E, I, O, U), H, W, and Y after the first letter
  name <- paste0(first_letter, gsub("[AEIOUHWY]", "", substr(name, 2, nchar(name))))

  # Pad with zeros or trim to ensure the result is exactly 4 characters long
  substr(paste0(name, "000"), 1, 4)
}

case_sensitive = FALSE
ignore_whitespace = TRUE
frag_size = 2
