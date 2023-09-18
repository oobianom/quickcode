#' Yes/No to 1/0 or to TRUE/FALSE or vice versa
#'
#' @export
#'
bool <- function(., conv.to = c(1, TRUE, "Yes")) {
  # match conversion to
  conv.to <- match.arg(conv.to)

  # standardize to binary
  switch(tolower(.),
    "true" = 1,
    "yes" = 1,
    "no" = 0,
    "false" = 0
  )

  # return based on conv.to
}
