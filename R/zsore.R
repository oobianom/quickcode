#' Calculates Z-Scores of a distribution
#'
#' Calculates Z-Scores based on data
#'
#' @param data dataiable
#' @param round round output to how many decimal place description
#'
#' @examples
#' Capture z-scores from the following distribution x
#' x = c(6, 7, 7, 12, 13, 13, 15, 16, 19, 22)
#' z_scores = zscore(x, round = 2) # limit to 2 decimal place
#' z_scores = zscore(x) # no decimal place limit
#'
#' df = data.frame(val = x, zscore = z_scores)
#' head(df)
#' @export

zscore <- function(data, round) {
  .x <- (data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)
  if(!missing(round)){
   .x <-  round(.x, round)
  }
  .x
}
