#' Convert Dates into Numeric Week Counts
#'
#' Convert the range of date to number of weeks
#'
#' @param date A variable of dates that need not be sequential but that reference values of class Date; if this argument is populated, the start_date and end_date arguments must be empty
#' @param start_date A scaler of class Date (if this argument is populated, the date arg must be empty)
#' @param end_date A scaler of class Date; must be later than the start_date (if this argument is populated, the date arg must be empty)
#' @param seq = The sequential method used to generate the data frame; the options for configuring this argument are as follows:
#' \cr\cr - bywk
#' \cr\cr - byseq
#' \cr\cr - bycont
#' @note
#' The default configuration for the seq argument should be "bywk."
#'
#' @export
#'

getWeekSeq <- function(start_date, end_date, date, seq) {
  # Convert input dates to Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Generate a sequence of dates
  date_sequence <- seq(from = start_date, to = end_date, by = "day")

  # Create a data frame
  week_df <- data.frame(DATE = date_sequence)

  # Add a column for the week's sequence starting with the numeric
  # week of the year as provided by lubridate's week function
  sequence = lubridate::week(week_df$DATE)
  week_df$WEEK <- paste("WEEK", sequence)

  return(week_df)
}
