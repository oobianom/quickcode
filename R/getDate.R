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
#' @examples
#' str1 <- "The video was recorded on July 19, 2023."
#' str2 <- "The video was recorded over a 4 hour period
#' starting on July 19, 2023."
#' str3 <- "The first batch aug 12,2024 of aug 12,
#' 2024 reports are due on July 12, 2024; the second
#' batch on 7/19/24."
#' str4 <- c("On 3.12.25, Jerry is taking one month
#' 05.13.1895 of leave and is not scheduled to
#' return until around 4-9-2025.", "The staff
#' will be out on training on 10/11/24,
#' Oct 12, 2024, and 10-13-24.")
#' getDate(str1)
#' getDate(str2, out.format = "%Y-%m-%d")
#' getDate(str3, out.format = "%m-%d/%Y")
#' getDate(str4)
#'
getDate <- function(str1, out.format = "%Y-%m-%d") {
  # Match various date patterns using regex
  dt_pattern <- "(\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}|\\d{1,2}-[A-Za-z]{3}-\\d{2,4}|[A-Za-z]+ \\d{1,2},?\\s*\\d{4}|\\d{8}|\\d{6}|\\d{1,2}\\.\\d{1,2}\\.\\d{2,4})"

  # Extract all occurrences of a date
  extracted_dt <- regmatches(str1, gregexpr(dt_pattern, str1))

  # Convert dates to an object of Class Date
  # Return the extracted dates as a list object (or character(0) if not found)
  output.date <- lapply(extracted_dt, function(m) {
    .to..date(m, out.format = out.format)
  })
  list(raw = extracted_dt, transformed = output.date)
}




# Function to convert any date format to Month-Day-Year (MM-DD-YYYY)
# Example usage
# dates <- c("aug 12,1902", "aug 12, 1986", "July 12, 2024", "7/19/24")

.to..date <- function(dae, out.format) {

  # Try to convert the date using the defined formats
  res <- c()
  for (ut in trimws(dae)) {
  resp <- c(NA)
  respm <- c(-1)
    for (fmt in .date_stringi) {
      parsed_date <- tryCatch(as.Date(ut, format = fmt), error = function(e) NA)
      if (!is.na(parsed_date) & !grepl("^00",parsed_date)) {
        j = format(parsed_date, out.format)
        vector_push(resp, j)
        vector_push(respm, paste0(percent_match(j,ut)$overall_match_percent," - ",fmt))
        #break
      }
    }
    vector_push(res, resp[which(respm == max(respm))])
  }
  res
  # If none of the formats worked, return NA or an error message
  # stop(paste0("Unable to parse date. Please check the format. check: ",dae))
}




.date_stringi <- c(
  "%D","%F",
  "%B %d, %Y",  # "August 15, 2024"
  "%b %d, %Y",  # "Aug 15, 2024"
  "%m/%d/%Y",   # "08/15/2024"
  "%m/%d/%y",   # "08/15/24"
  "%d-%m-%Y",   # "15-08-2024"
  "%d-%m-%y",   # "15-08-24"
  "%m-%d-%Y",   #
  "%m-%d-%y",   #
  "%Y-%m-%d",   # "2024-08-15"
  "%d %B %Y",   # "15 August 2024"
  "%d %b %Y",   # "15 Aug 2024"
  "%B %d %Y",   # "August 15 2024"
  "%b %d %Y",   # "Aug 15 2024"
  "%y-%m-%d",   # "24-08-15"
  "%Y/%m/%d",   # "2024/08/15"
  "%d/%m/%Y",   # "15/08/2024"
  "%A, %B %d, %Y", # "Thursday, August 15, 2024"
  "%a, %b %d, %Y", # "Thu, Aug 15, 2024"
  "%m%d%Y",     # "08152024"
  "%m%d%y",     # "081524"
  "%Y.%m.%d",   # "2024.08.15"
  "%y.%m.%d",   # "24.08.15"
  "%d.%m.%Y",   # "15.08.2024"
  "%d.%m.%y",   # "15.08.24"
  "%m.%d.%Y",   #
  "%m.%d.%y",   #
  "%Y %m %d",   # "2024 08 15"
  "%y %m %d"    # "24 08 15"
)
