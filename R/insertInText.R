#' Shiny app function to insert string to current file in RStudio
#'
#' Shorthand to insert content to opened file
#'
#' @param string what to insert
#' @return Inserts into current position on opened file
#'
#' @examples
#' if(interactive()){
#' insertInText('hello rpkg.net')
#' insertInText('hello world')
#' }
#' @export
#'

insertInText <- function(string) {
  adc <- rstudioapi::getActiveDocumentContext()
  rstudioapi::insertText(location = adc$selection[[1]]$range$start, string)
}
