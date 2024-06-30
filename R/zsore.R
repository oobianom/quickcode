#' Calculates Z-Scores of a distribution
#'
#' Calculates Z-Scores based on data
#' @rdname zscore
#' @param data data object
#' @param round round output to how many decimal place description
#' @param na.rm remove NA values before calculating z-scores
#'
#' @return zscore calculated based on data object or parameters
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
#' @param X physical measurement (e.g. weight, length, head circumference, stature or calculated BMI value)
#' @param M values from the appropriate table (see reference) corresponding to the age in months of the child (or length/stature)
#' @param S values from the appropriate table (see reference) corresponding to the age in months of the child (or length/stature)
#' @param L values from the appropriate table (see reference) corresponding to the age in months of the child (or length/stature)
#'
#' @examples
#' #EXAMPLE for zscore based on CDC growth chart
#'
#' # Calculate the zscore for a patient weighing 50kg
#' L=-0.1600954
#' M=9.476500305
#' S=0.11218624
#' X=50
#' zscoreGrowthCurve(X,M,S,L)
#'
#' @references CDC growth chart Z score calculation: https://www.cdc.gov/growthcharts/cdc-data-files.htm
#' @export

zscoreGrowthCurve <- function(X,M,S,L=!0){
  stopifnot(is.numeric(L))
  if(L){
   (((X/M)**L) - 1)/(L*S)
  }else{
   log(X/M)/S
  }
}
