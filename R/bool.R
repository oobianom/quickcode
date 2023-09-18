#' Yes/No to 1/0 or to TRUE/FALSE or vice versa
#'
#' @details
#' conv.to: 1 - yes/no, 2 - TRUE/FALSE, 3 - 1/0
#'
#' @export
#'
as.boolean <- function(., conv.to = 1:3) {
  # match conversion to
  conv.to <- match.arg(conv.to)
  print(conv.to)
  .lower <- tolower(.)
  # standardize to binary
  .res <- switch(.lower,"true" = 1,"t" = 1,"yes" = 1,"y" = 1,"no" = 0, "n" = 0, "false" = 0, "f" = 0 )
  .res <- switch(.res,"1" = 1,"2" = 1,"3" = 1)

  # return based on conv.to
}
