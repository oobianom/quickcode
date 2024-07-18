#' Cyber security functions
#'
#' Get all active connections
#'
#' @return a data frame containing the list of all connections
#'
#' @examples
#' \donttest{
#' # Get a list of all connections on my system
#'
#' gAc <- get_active_conn()
#' }
#'
#' @rdname cybersecurity
#'
# get_active_conn <- function(){
#     # Retrieve netstat output
#     netstat_output <- system("netstat -a -n -o", intern = TRUE)
#
#     # Formalize vector structure
#     netstat_output <- netstat_output[4:length(netstat_output)]
#
#     # Rename line columns
#     netstat_output[1] <- "  Proto  Local_Address          Foreign_Address        State           PID"
#
#     # Split the lines into columns
#     data <- strsplit(netstat_output, "\\s+")
#
#     # Create a matrix
#     df = do.call(rbind.data.frame, data)[,-1]
#
#     # Set column names
#     colnames(df) <- c("Proto", "Local_Address", "Foreign_Address", "State", "PID")
#
#     # Reset dataframe
#     df[-c(1),]
#   }


#' @rdname cybersecurity
#' @param input input string containing IP
#' @return extract_IP returns a vector of IP addresses
#'
#' @examples
#' \donttest{
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
#' }
#'

# extract_IP <- function(input) {
#   # Match an IP address pattern
#   ip_pattern <- "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b"
#
#   # Extract all occurrences of IP addresses
#   extracted_ips <- gregexpr(ip_pattern, input)
#
#   # Deconstruct the list object
#   # Return the extracted IP addresses (or an empty list if not found)
#   unlist(regmatches(input, extracted_ips))
# }




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
  # Match an IP address pattern
  ip_pattern <- "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b"
  if(Sys.info()['sysname'] == "Linux"){
    loc <- grep("eth0$", system("ip -4 a", intern = TRUE))
    ip <- system("ip -4 a",intern = TRUE)[loc]
    # Extract the occurrence of an IP address
    # Return the extracted IP address (or character(0) if not found)
    regmatches(ip, regexpr(ip_pattern, ip))
  }
  if(Sys.info()['sysname'] == "Windows"){
    # Get vector element of machine's local IP address
    loc <- grep("IPv4", system("ipconfig", intern = TRUE))
    ip <- system("ipconfig",intern = TRUE)[loc]

    # Extract the occurrence of an IP address
    # Return the extracted IP address (or character(0) if not found)
    regmatches(ip, regexpr(ip_pattern, ip))
  }
}


