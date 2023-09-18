#' Yes/No to 1/0 or to TRUE/FALSE or vice versa
#'
#' @details
#' conv.to: 1 - yes/no, 2 - TRUE/FALSE, 3 - 1/0
#'
#' @export
#'
as.boolean <- function(., conv.to = c1t3) {
  # match conversion to
  conv.to <- match.arg(conv.to)
  print(conv.to)
  # standardize to binary
  .. <- switch(tolower(.),"true" = 1,"t" = 1,"yes" = 1,"y" = 1,"no" = 0, "n" = 0, "false" = 0, "f" = 0 )
  # return based on conv.to
  ifelse(..,switch(..,"1" = "Yes","2" = TRUE,"3" = 1),switch(..,"1" = "No","2" = FALSE,"3" = 0))
}
