#' Extract all IP addresses from a string
#'
#' Parse a string and vectorize the IP addresses within it
#'
#' @rdname cybersecurity
#' @param input input string containing IP
#' @return extract_IP returns a vector of IP addresses
#'
#' @examples
#' \donttest{
#' # Extract all IP addresses from a string
#'
#' # Example 1
#' # This example demonstrates the separate
#' # extraction of an IP address one per vector element:
#' str1 = c("Two IP(66.544.33.54) addresses
#' were discovered in the scan.
#' One was at 92.234.1.0.",
#' "The other IP was 62.3.45.255.")
#' extract_IP(str1)
#'
#' # Example 2
#' # This example demonstrates the extraction
#' # of multiple IP addresses from a single vector element:
#' str2 = "Two IP addresses were discovered
#' in the scan. One was at 92.234.1.0.
#' The other IP was 62.3.45.255."
#' extract_IP(str2)
#' }
#' @export

extract_IP <- function(input) {
  # Match an IP address pattern
  ip_pattern <- "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b"

  # Extract all occurrences of IP addresses
  extracted_ips <- gregexpr(ip_pattern, input)

  # Deconstruct the list object
  # Return the extracted IP addresses (or an empty list if not found)
  unlist(regmatches(input, extracted_ips))
}

