#' Track Function Usage and Performance
#'
#' Up count the number of times a particular function is called. This function tracker is a higher-order function or decorator that wraps around other functions to monitor and record their execution patterns, including metrics like call frequency, execution time, argument patterns, and return values. It acts as a transparent layer that doesn't modify the original function's behavior but collects valuable metadata about its usage.
#'
#' @param output.dest destination of csv file to store outputs
#' @return the numeric count of a function usage
#' @note
#' The usefulness of function tracking spans several critical areas:\cr\cr
#' \strong{Performance Optimization}: By measuring execution times and frequency, developers can identify bottlenecks and frequently called functions that need optimization\cr
#' \strong{Debugging}: Tracking argument patterns and function call sequences helps pinpoint issues in complex applications\cr
#' \strong{Usage Analytics}: Understanding which features (functions) are most commonly used helps guide development priorities and API design decisions\cr
#' \strong{Resource Management}: Monitoring function behavior helps identify memory leaks, resource consumption patterns, and potential optimization opportunities\cr
#' \strong{Testing}: Usage patterns can inform test case design and coverage requirements, ensuring critical paths are well-tested\cr
#' \strong{Documentation}: Automatically gathering real-world usage examples helps maintain accurate and relevant documentation\cr
#' \strong{Compliance}: In regulated environments, function tracking can help maintain audit trails of system behavior\cr
#' @examples
#' \dontrun{
#' library(quickcode)
#' # Track usage of type2 and type1 functions
#' store.usage.file <- tempfile()
#' type5 <- function(x) type2(x)
#' type4 <- function(x) type3(x)
#' type3 <- function(x) type1(x)
#' type1 <- function(x) {
#'   mean(x)
#'   sd(x)
#'   track_func(store.usage.file)
#' }
#' type2 <- function(x) {
#'   type1(x)
#'   track_func(store.usage.file)
#' }
#'
#' # add usage counts to store.usage.file
#' type1(number(10))
#' type2(number(10))
#' type3(number(10))
#' type4(number(10))
#' type5(number(10))
#'
#' # check the stored function usage file
#' print(read.csv(store.usage.file))
#' }
#' @export
track_func <- function(output.dest = "output_tracking.csv") {
  getCall <- as.character(sys.calls()[[length(sys.calls()) - 1]])
  getFuncName <- strsplit(getCall, "\\(")[[1]][1]
  appenddf <- data.frame(Function = getFuncName, Usage = 1)
  if (file.exists(output.dest)) {
    out.f <- read.csv(output.dest, header = T)
    match.f <- subset(out.f, Function == getFuncName)
    if (nrow(out.f[out.f$Function == getFuncName, ])) {
      out.f[out.f$Function == getFuncName, ]$Usage <- out.f[out.f$Function == getFuncName, ]$Usage + 1
    } else {
      out.f <- rbind(out.f, appenddf)
    }
    utils::write.csv(out.f, output.dest, quote = FALSE, row.names = FALSE)
  } else {
    utils::write.csv(appenddf, output.dest, quote = FALSE, row.names = FALSE)
  }
}

#' Extract all comments or functions from a file
#'
#' Vectorize all comments from the file
#' @rdname comments
#' @param file path of file to use for processes
#' @return vector of all comments within a file
#' @examples
#' \dontrun{
#' ex_file1 <- "path/file1.R"
#' # get all comments
#' cmmts <- extract_comment(ex_file1)
#' cmmts
#' }
#'
#' @param file file to parse
#' @export

extract_comment <- function(file) {
  # Read the file line by line
  lines <- readLines(file)

  # Initialize a vector to store comments
  comments <- c()

  # Loop through each line to extract comments
  for (line in lines) {
    # Extract comments from the beginning of the line
    line <- remove_content_in_quotes(line)
    if (grepl("^\\s*#", line)) {
      comments <- c(comments, line)
    }
    # Extract inline comments
    inline_comments <- unlist(regmatches(line, gregexpr("#.*", line)))
    if (length(inline_comments) > 0 && !grepl("^\\s*#", line)) {
      comments <- c(comments, inline_comments)
    }
  }

  # Return the comments
  return(comments)
}

#' @rdname comments
#' @param line string vector to remove contents within quotes or comments
#' @export
remove_content_in_quotes <- function(line) {
  # Remove content within single quotes
  line <- gsub("'[^']*'", "", line)
  # Remove content within double quotes
  line <- gsub('"[^"]*"', "", line)
  return(line)
}


#' @rdname comments
#' @export
remove_comment <- function(line) {
  stopifnot(length(line) == 1)
  comment_index <- regexpr("#", line)
  if (comment_index == -1) line else gsub("#.*$", "", line)
}


# Function to numb internal comments
hide_int_cmts <- function(string) {
  string <- gsub('("[^"]*)#+([^"]*)"', '\\10x5&9%80x\\2"', string, perl = TRUE)
  string <- gsub("('[^']*)#+([^']*)'", "\\10x5&9%80x\\2'", string, perl = TRUE)
  string
}


#' @rdname comments
#' @param output_file file path to write the output of the new file
#' @export
#' @examples
#' \dontrun{
#' # Ex to clean out comments from file
#' file_path <- ".testR"
#' output_file_path <- ".cleaned_script.R"
#' clean_file(file_path, output_file_path)
#' }
#'
clean_file <- function(file, output_file) {
  # Read the file line by line
  lines <- readLines(file)

  # Process each line: remove comments and empty lines
  cleaned_lines <- sapply(lines, function(line) {
    line <- hide_int_cmts(line)
    line <- gsub(master_file_clean_sep, "#", remove_comment(line))
    line <- trimws(line) # Remove leading and trailing whitespace
    if (nchar(line) > 0) {
      return(line)
    } else {
      return(NULL)
    }
  })
  res <- as.character(cleaned_lines[!sapply(cleaned_lines, is.null)])
  if (missing(output_file)) res else writeLines(res, output_file)
}


#' Get all defined functions within a file
#'
#' @examples
#' # example code
#'
#' \dontrun{
#' # Ex to get all defined functions
#' # within a file
#' file_path <- ".testR"
#' get_func_def(file_path)
#' }
#' @rdname comments
#' @export
get_func_def <- function(file) {
  code <- clean_file(file)
  .envi <- new.env()
  eval(parse(text = code), envir = .envi)
  ls(envir = .envi)
}
