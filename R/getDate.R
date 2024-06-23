#' @export


getDate <- function(vec) {
  # Match various date patterns using regex
  dt_pattern <- "(\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}|\\d{1,2}-[A-Za-z]{3}-\\d{2,4}|[A-Za-z]+ \\d{1,2},? \\d{4}|\\d{8}|\\d{6}|\\d{1,2}\\.\\d{1,2}\\.\\d{2,4})"

  # Extract all occurrences of a date
  extracted_dt <- regmatches(vec, gregexpr(dt_pattern, vec))

  # Convert dates to an object of Class Date
  # Return the extracted dates as a list object (or character(0) if not found)
  lapply(extracted_dt, easyr::todate)
}


#' @rdname cybersecurity
#' @param input input string containing IP
#' @export

extract_IP <- function(input) {
  # Match an IP address pattern
  ip_pattern <- "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b"

  # Extract all occurrences of IP addresses
  extracted_ips <- stringr::str_extract_all(input_string, ip_pattern)

  # Deconstruct the list object
  # Return the extracted IP addresses (or an empty list if not found)
  unlist(extracted_ips)
}
