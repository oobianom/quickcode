#' Convert Dates into Numeric Week Counts
#'
#' Convert the range of date to number of weeks
#' @rdname date-topic
#' @param dates vector of dates that need not be sequential but that reference values of class Date;
#' Note that if this argument is populated, the start_date and end_date arguments must be empty
#' @param start_date A scaler of class Date (if this argument is populated, the date arg must be empty)
#' @param end_date A scaler of class Date; must be later than the start_date (if this argument is populated, the date arg must be empty)
#' @param in.format date input format
#' @return data frame of the dates along with their corresponding week
#' @examples
#' # simple example with start and end date
#' getWeekSeq(start_date="12/29/25",end_date="1/8/26")
#'
#' # enter specific dates instead
#' # specify format
#' getWeekSeq(
#' dates = c(
#'   '2025-12-29',
#'   '2025-12-30',
#'   '2025-12-31',
#'   '2026-01-01',
#'   '2026-01-04',
#'   '2026-01-05',
#'   '2026-01-06',
#'   '2026-01-07',
#'   '2026-01-08'),
#'   in.format = "%Y-%m-%d"
#' )
#'
#' getWeekSeq(
#' dates = c(
#'   '12/29/25',
#'   '12/30/25',
#'   '12/31/25',
#'   '01/01/26',
#'   '01/02/26',
#'   '01/03/26',
#'   '01/06/26',
#'   '01/07/26',
#'   '01/08/26'
#'   ),
#'   in.format = "%m/%d/%y"
#' )
#'
#' @export
#'

getWeekSeq <- function(start_date, end_date, dates, in.format = "%m/%d/%y") {
  if(!missing(dates)){
    date_sequence <- as.Date(dates, format=in.format)
    if(!missing(start_date) | !missing(end_date))
      stop("'start_date' and 'end_date' should not be provided if 'dates' is provided")
  }else{
    # Convert input dates to Date objects
    start_date <- as.Date(start_date, format=in.format)
    end_date <- as.Date(end_date, format=in.format)
    # Generate a sequence of dates
    date_sequence <- seq(from = start_date, to = end_date, by = "day")
  }

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


