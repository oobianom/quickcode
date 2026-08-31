#' Re-sample a dataset by column and return number of entry needed
#'
#' Shorthand to return a re-sample number of rows in a data frame by unique column
#'
#' @param .dt data frame to re-sample
#' @param col column to uniquely re-sample
#' @param n number of rows to return
#' @param seed unique numeric value for reproducibility
#' @param replace should sampling be with replacement
#' @return data frame containing re-sampled rows from an original data frame
#'
#' @examples
#' data1 <- data.frame(ID=1:10,MOT=11:20)
#' sample_by_column(data1,MOT,3)
#' sample_by_column(data1,ID,7)
#' @export
#'

sample_by_column <- function(.dt, col, n, seed = NULL, replace = FALSE) {
  if(not.null(seed))set.seed(seed)
  warning("Work on function still in progress. Use with caution.")
  if(not.data(.dt)) stop("First element must be a data frame.")
  .dt[.dt[, as.character(substitute(col))] %in% sample(unique(.dt[, as.character(substitute(col))]), n, replace = replace),][1:n,]
}
