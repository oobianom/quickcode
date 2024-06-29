#' Convert Dates into Numeric Week Counts
#'
#' Convert the range of date to number of weeks
#' @rdname date-topic
#' @param start_date A scaler of class Date (if this argument is populated, the date arg must be empty)
#' @param end_date A scaler of class Date; must be later than the start_date (if this argument is populated, the date arg must be empty)
#' @param in.format date input format
#' @return data frame of the dates along with their corresponding week
#' @examples
#' getWeekSeq(start_date="12/29/25",end_date="1/8/26")
#'
#' @export
#'

getWeekSeq <- function(start_date, end_date, in.format = "%m/%d/%y") {
  # Convert input dates to Date objects
  start_date <- as.Date(start_date, format=in.format)
  end_date <- as.Date(end_date, format=in.format)

  # Generate a sequence of dates
  date_sequence <- seq(from = start_date, to = end_date, by = "day")

  # Create a data frame
  week_df <- data.frame(DATE = date_sequence)

  # Add a column for the week's sequence starting with the numeric
  # week of the year as provided by lubridate's week function
  week_df$WEEK <- weeki(week_df$DATE)

  week_df
}



weeki <- function (x)
{
  (yday1(x) - 1)%/%7 + 1
}

yday1 <- function(date) {


  year <- as.numeric(format(start, "%Y"))
  month <- as.numeric(format(date, "%m"))
  day <- as.numeric(format(date, "%d"))

  # Calculate days in months
  monthsDays <- function(year) {
    days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    if(year%%400 == 0 || (year%%100 != 0 && year%%4 == 0)) days[2] <- 29
    days
  }

  days_in_months <- function(year) {
    cumsum(c(0, monthsDays(year)))
  }

  # Calculate day of year
  doy <- days_in_months(year)[month] + day - 1

  return(doy)

}

#' @rdname date-topic
#' @export
is.Date <- function(x){
  inherits(x, "Date")
}
