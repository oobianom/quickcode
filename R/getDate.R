# Future release 1.0
#' Extract all dates from a string
#' @param out.format date output format
#' @param str1 string to parse
#' @note
#' \strong{DATE FORMATS IN R}\cr
#' \tabular{rrrrr}{
#' \strong{Date Specification}   \tab \tab \strong{Description}          \tab \tab  \strong{Example} \cr
#' \%a  \tab \tab Abbreviated weekday             \tab \tab Sun, Thu \cr
#' \%A  \tab \tab Full weekday                    \tab \tab Sunday \cr
#' \%b  \tab \tab Abbreviated month               \tab \tab May, Jul \cr
#' \%B  \tab \tab Full month                      \tab \tab March, July \cr
#' \%d  \tab \tab Day of the month                \tab \tab 27, 07 \cr
#' \%j  \tab \tab Day of the year                 \tab \tab 148, 188 \cr
#' \%m  \tab \tab Month                           \tab \tab 05, 07 \cr
#' \%U  \tab \tab Week, with Sunday as first day  \tab \tab 22, 27 \cr
#' \%w  \tab \tab Weekday, Sunday is 0            \tab \tab 0, 4 \cr
#' \%W  \tab \tab Week, with Monday as first day  \tab \tab 21, 27 \cr
#' \%x  \tab \tab Date, locale-specific           \tab \tab \cr
#' \%y  \tab \tab Year without century            \tab \tab 84, 05 \cr
#' \%Y  \tab \tab Year with century               \tab \tab 1984, 2005 \cr
#' \%C  \tab \tab Century                         \tab \tab 19, 20 \cr
#' \%D  \tab \tab Date formatted \%m/\%d/\%y      \tab \tab 07/17/23 \cr
#' \%u  \tab \tab Weekday, Monday is 1            \tab \tab 7, 4 \cr
#' }
#'
#' @export
getDate <- function(str1,out.format = "%Y-%m-%d") {
  # Match various date patterns using regex
  dt_pattern <- "(\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}|\\d{1,2}-[A-Za-z]{3}-\\d{2,4}|[A-Za-z]+ \\d{1,2},? \\d{4}|\\d{8}|\\d{6}|\\d{1,2}\\.\\d{1,2}\\.\\d{2,4})"

  # Extract all occurrences of a date
  extracted_dt <- regmatches(str1, gregexpr(dt_pattern, str1))

  # Convert dates to an object of Class Date
  # Return the extracted dates as a list object (or character(0) if not found)
  output.date <- lapply(extracted_dt, function(m){
    print(m)
    as.POSIXct(easyr::todate(m),format="%Y-%m-%d")
    #do.call("as.POSIXct",c(easyr::todate(m)))
    easyr::todate(m)
    })
  #format(output.date, format = out.format)
}





