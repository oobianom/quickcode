#' Cyber security functions
#'
#' Get all active connections
#'
#' @export
#' @return a data frame containing the list of all connections
#'
#' @examples
#'
#' # Get a list of all connections on my system
#' get_active_conn()
#'
#'
#' @rdname cybersecurity
#'
get_active_conn <- function(){
    # Retrieve netstat output
    netstat_output <- system("netstat -a -n -o", intern = TRUE)

    # Formalize vector structure
    netstat_output <- netstat_output[4:length(netstat_output)]

    # Rename line columns
    netstat_output[1] <- "  Proto  Local_Address          Foreign_Address        State           PID"

    # Split the lines into columns
    data <- strsplit(netstat_output, "\\s+")

    # Create a matrix
    m <- stringi::stri_list2matrix(x = data, byrow = TRUE, fill = 0)

    # Convert to data frame
    df <- data.frame(m)

    # Remove empty column
    df$X1 <- NULL

    # Set column names
    colnames(df) <- c("Proto", "Local_Address", "Foreign_Address", "State", "PID")

    # Reset dataframe
    df[-c(1),]
  }


#' @rdname cybersecurity
#' @param input input string containing IP
#' @return extract_IP returns a vector of IP addresses
#'
#' @examples
#' # Extract all IP addresses from a string
#'
#' # Example 1
#' x = system("ipconfig /all", intern = TRUE)
#' extract_IP(x)
#'
#' # Example 2
#' # This example demonstrates the separate
#' # extraction of an IP address one per vector element:
#' str1 = c("Two IP(66.544.33.54) addresses
#' were discovered in the scan.
#' One was at 92.234.1.0.",
#' "The other IP was 62.3.45.255.")
#' extract_IP(str1)
#'
#' # Example 3:
#' # This example demonstrates the extraction
#' # of multiple IP addresses from a single vector element:
#' str2 = "Two IP addresses were discovered
#' in the scan. One was at 92.234.1.0.
#' The other IP was 62.3.45.255."
#' extract_IP(str2)
#'
#' @export

extract_IP <- function(input) {
  # Match an IP address pattern
  ip_pattern <- "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b"

  # Extract all occurrences of IP addresses
  extracted_ips <- stringr::str_extract_all(input, ip_pattern)

  # Deconstruct the list object
  # Return the extracted IP addresses (or an empty list if not found)
  unlist(extracted_ips)
}
