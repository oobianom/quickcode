#' Get the IP address of the current user
#'
#' Obtain my local IP address
#'
#' @return local IP address numeric value
#'
#' @examples
#' \donttest{
#' # Fetch my IP address
#' getMyIP() # show my current IP address
#' }
#'
#'
#' @rdname cybersecurity
#' @export
#'
getMyIP <- function() {
  # Get vector element of machine's local IP address
  loc <- grep("IPv4", system("ipconfig", intern = TRUE))
  ip <- system("ipconfig",intern = TRUE)[loc]

  # Match an IP address pattern
  ip_pattern <- "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b"

  # Extract the occurrence of an IP address
  # Return the extracted IP address (or character(0) if not found)
  regmatches(ip, regexpr(ip_pattern, ip))
}
