#' Calculates Z-Scores of a distribution
#'
#' Calculates Z-Scores based on data
#' @rdname zscore
#' @param data dataiable
#' @param round round output to how many decimal place description
#' @param na.rm remove NA values before calculating z-scores
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

zscore <- function(data, round, na.rm = TRUE) {
  .x <- (data - mean(data, na.rm = na.rm)) / sd(data, na.rm = na.rm)
  if(!missing(round)){
   .x <-  round(.x, round)
  }
  .x
}



#' @rdname zscore
#'
#' @export

ZscoreGrowthCurve <- function(X,M,S,L=!0){
  stopifnot(is.numeric(L))
  if(L){
   (((X/M)**L) - 1)/(L*S)
  }else{
   ln(X/M)/S
  }
}
