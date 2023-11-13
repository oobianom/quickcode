#' Generate a bionic text
#'
#' This function serves as a mechanism
#' enabling the conversion of provided
#' text into a bionic form. Users input
#' the text, and the function, in turn,
#' delivers the text transformed into a
#' bionic format.
#'
#' @param text input text
#' @param verbose	a logical indicating if to display result in console
#'
#' @details
#' A bionic text refers to a transformed version
#' of a given text achieved through a specialized
#' function designed to incorporate elements of
#' advanced technology, enhancing both the form
#' and content of the original input. This function
#' operates by infusing the text with a fusion of
#' various elements, resulting in a synthesis
#' that transcends traditional linguistic boundaries.
#' The function augments the text
#' with dynamic visual representations that adapt to
#' the reader's preferences.
#' The goal is to create a text that not only conveys
#' information but also engages the audience in a more
#' immersive and interactive manner, harnessing
#' the capabilities of modern technology to redefine
#' the traditional concept of textual communication.
#' An example of a bionic text could be a news article
#' that dynamically updates with real-time data,
#' incorporates multimedia elements, and adjusts its
#' presentation style based on the reader's preferences,
#' thereby offering a more enriched and personalized reading
#' experience.
#'
#'
#' @return bionic text
#'
#' @examples
#' # simple example to show a text
#' # transformation to bionic text
#'
#' # text to transform
#' text1 <- "A tool for nonparametric
#' estimation and inference
#' of a non-decreasing
#' monotone hazard\nratio
#' from a right censored survival dataset."
#'
#' # transform text, save to variable and print later
#' # as a message
#' genbt <- bionic_txt(text1)
#' message(genbt)
#'
#' # transform text and print directly in console
#' bionic_txt(text1)
#'
#'
#' @export

bionic_txt <- function(text, verbose = TRUE) {
  words <- unlist(strsplit(text, " "))
  modified_words <- sapply(words, modify_word)
  formatted_text <- paste(modified_words, collapse = " ")
  if(verbose) cat(formatted_text, "\n")
  else paste0(formatted_text, "\n")
}



