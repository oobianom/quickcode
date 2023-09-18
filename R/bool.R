#' Yes/No to 1/0 or to TRUE/FALSE or vice versa
#'
#' @export
#'
bool <- function(., conv.to = c(1, TRUE, "Yes")) {
  # match conversion to
  conv.to <- match.arg(conv.to)
  .lower <- tolower(.)

  .bin1 <- ifelse(grep("^((yes)|(true)|(1))$",.),1,
         ifelse(grep("^((no)|(false)|(0))$",.),0,NA))

  # standardize to binary
  # switch(tolower(.),
  #   "true" = 1,
  #   "yes" = 1,
  #   "no" = 0,
  #   "false" = 0
  # )

  # return based on conv.to
  .bin1
}
