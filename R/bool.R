#' Yes/No to 1/0 or to TRUE/FALSE or vice versa
#'
#' @param . item to convert
#' @param conv.to format to convert to, choices 1, 2 or 3
#' @details
#' conv.to: options are 1 - yes/no, 2 - TRUE/FALSE, 3 - 1/0
#'
#' @examples
#' # convert 1 to format of Yes or No
#' as.boolean(1,1)
#'
#' # convert "T" to format of Yes or No
#' as.boolean("T",1)
#'
#'
#' # convert "f" to format of TRUE or FALSE
#' as.boolean("f",2)
#'
#'
#' # convert 1 to format of TRUE or FALSE
#' as.boolean(1,2)
#'
#'
#' # convert "Y" or "y" to format of Yes or No
#' as.boolean("Y",1) #uppercase Y
#' as.boolean("y",1) #lowercase y
#'
#'
#' # convert TRUE/FALSE to format of 1 or 0
#' as.boolean(TRUE,3)
#' as.boolean(FALSE,3)
#'
#'
#' # convert TRUE/FALSE to format of Yes or No
#' as.boolean(TRUE,1)
#' as.boolean(FALSE,1)
#'
#'
#'
#' # in case of error in argument
#' as.boolean("tr",3) #logical(0)
#' as.boolean("ye",3) #logical(0)
#'
#'
#' @export
#'
as.boolean <- function(., conv.to) {
  # standardize to binary
  .. <- switch(tolower(.),"true" = 1,"t" = 1,"yes" = 1,"y" = 1,"no" = 0, "n" = 0, "false" = 0, "f" = 0,"1" = 1,"0" = 0)
  # return based on conv.to
  h = ifelse(..,switch(conv.to,"1" = "Yes","2" = TRUE,"3" = 1),switch(conv.to,"1" = "No","2" = FALSE,"3" = 0))
  gg<<-h
  h
}
