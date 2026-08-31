#' Snippet R function to clear console and set directory
#'
#' Shorthand to add clear console code to current file
#'
#' @return Inserts code to clear console
#'
#' @examples
#' if(interactive())
#' add.snippet.clear()
#' @export
#'

add.snippet.clear <- function() {
  insertInText(paste0("
# script header
quickcode::libraryAll(
  ...
)
quickcode::clean(
  setwd = './'
)

# script body





# session information
  sessionInfo()
"))
}
