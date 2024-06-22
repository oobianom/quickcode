#' Cyber security functions
#'
#' Get all active connections
#'
#' @export
#' @return a data frame containing the list of all connextions
#'
#' @examples
#'
#' # Get all connections on my system
#' x = get_active_conn()
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
    df <- df[-c(1),]

    return(df)
  }
