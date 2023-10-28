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



