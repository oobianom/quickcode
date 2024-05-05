#' Derive Numeric Week Based on Dates
#'
#' Convert the range of date to number of weeks
#'
#' @param start_date the start date
#' @param end_date the end date
#'
#' @export
#'

getWeekSeq <- function(start_date, end_date) {
  # Convert input dates to Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Generate a sequence of dates
  date_sequence <- seq(from = start_date, to = end_date, by = "day")

  # Create a data frame
  week_df <- data.frame(DATE = date_sequence)

  # Add a column for the week's sequence starting with the numeric week of the year as provided by lubridate's week function
  sequence = lubridate::week(week_df$DATE)
  week_df$WEEK <- paste("WEEK", sequence)

  return(week_df)
}
