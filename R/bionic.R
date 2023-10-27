#' Generate bionic text
#'
#' Generate a bionic text
#'
#' @param input_text input text
#'
#' @return bionic text
#'
#' @export

bionic_txt <- function(input_text) {
  words <- unlist(strsplit(input_text, " "))
  modified_words <- sapply(words, modify_word)
  formatted_text <- paste(modified_words, collapse = " ")
  cat(formatted_text, "\n")
}


modify_word <- function(word) {
  bold <- "\033[1m"
  underline <- "\033[4m"
  reset <- "\033[0m"
  blue <- "\033[34m"
  word_length <- nchar(word)
  first_half <- substr(word, 1, ceiling(word_length / 2))
  first_half_bold <- paste0(bold, first_half, reset)
  second_half <- substr(word, ceiling(word_length / 2) + 1, word_length)
  second_half_bold <- paste0(blue, second_half, reset)
  final_word <- paste0(first_half_bold, second_half_bold)
  return(final_word)
}
