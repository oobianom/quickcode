#' Remove Empty Lines from a File
#'
#' This function reads a text file, removes all empty lines, and saves the
#' modified content back to the same file.
#'
#' @param file_path A character string specifying the path to the text file.
#' @param resave A logical value indicating if the file content should be resaved or returned
#'
#' @return NULL This function modifies the file in place and does not return a value.
#'
#' @examples
#' \donttest{
#' if(interactive()){
#' # Remove empty lines from a file
#' trim.file("path/to/your/file.txt")
#' }
#' }
#' @export
trim.file <- function(file_path, resave = TRUE) {
  if(missing(file_path)) file_path = rstudioapi::getActiveDocumentContext()$path
  # Read the file
  lines <- readLines(file_path)
  # Remove empty lines
  non_empty_lines <- lines[nzchar(lines)]
  message(paste0("Empty lines have been removed from ",basename(file_path)))
  # Write the non-empty lines back to the file
  if(resave) writeLines(non_empty_lines, file_path) else non_empty_lines
}
