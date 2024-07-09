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
  week_df$WEEK <- unlist(lapply(week_df$DATE, function(h){
    (yday2(h) - 1)%/%7 + 1
  }))
  week_df
}



yday2 = function(start_date){
  .year <- as.numeric(format(start_date,"%Y"))
  .month <- as.numeric(format(start_date,"%m"))
  .day <- as.numeric(format(start_date,"%d"))
  dinm <- c(0,31,28,31,30,31,30,31,31,30,31,30,31)
  if(is.leap(.year)) dinm[2] <- 29
  cumsum(dinm)[.month]+.day
}


#' @rdname date-topic
#' @param x date item to check
#' @export
is.Date <- function(x){
  inherits(x, "Date")
}
#' @rdname date-topic
#' @export
not.Date <- function(x)!{
  inherits(x, "Date")
}

#' @rdname date-topic
#' @param yyyy year numeric value eg 1989
#' @export
is.leap <- function(yyyy){
  if(missing(yyyy)) yyyy = as.numeric(format(Sys.Date(),"%Y"))
  yyyy = as.numeric(yyyy)
  unlist(lapply(yyyy, function(y1){
    y1%%4==0 && !(y1%%100==0 && y1%%400!=0)
  }))
}


