# to-do v1.0
# function to track function usage
type3 <-function(x)type1(x)
type1 <- function(x){
  mean(x)
  sd(x)
  track_function()
}

track_function <- function(apiId){
  getCall<-as.character(sys.calls()[[length(sys.calls())-1]])
  getFuncName <- strsplit(getCall,"\\(")[[1]][1]
  print(getFuncName)
  ls("package:quickcode")
}

#' Extract all comments from a file
#'
#' Vectorize all comments from the file
#'
#' @return vector of all comments within a file
#' @examples
#' \dontrun{
#' ex_file1 <- "path/file1.R"
#' # get all comments
#' cmmts <- extract_comments(ex_file1)
#' cmmts
#' }
#'
#' @param file file to parse
#' @export

extract_comments <- function(file) {
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

remove_content_in_quotes <- function(line) {
  # Remove content within single quotes
  line <- gsub("'[^']*'", "", line)
  # Remove content within double quotes
  line <- gsub('"[^"]*"', "", line)
  return(line)
}


hide_int_cmts <- function(string) {
  string <- gsub('("[^"]*)#+([^"]*)"', '\\1*5&9%8**\\2"', string, perl = TRUE)
  string <- gsub("('[^']*)#+([^']*)'", '\\1*5&9%8**\\2\'', string, perl = TRUE)
  string
}

clean_r_file <- function(file_path, output_file_path) {
  # Read the file line by line
  lines <- readLines(file_path)

  # Function to remove comments
  remove_comments <- function(line) {
    # Find the position of the first unquoted #
    comment_positions <- gregexpr("#", line)[[1]]
    for (pos in comment_positions) {
      if (pos != -1 && !is_inside_quotes(line, pos)) {
        return(substr(line, 1, pos - 1))
      }
    }
    return(line)
  }

  # Function to determine if a character is inside quotes
  is_inside_quotes <- function(line, pos) {
    single_quote_positions <- gregexpr("'", line)[[1]]
    double_quote_positions <- gregexpr('"', line)[[1]]

    quote_positions <- sort(c(single_quote_positions, double_quote_positions))
    inside_quote <- FALSE

    for (quote_pos in quote_positions) {
      if (quote_pos > pos) break
      inside_quote <- !inside_quote
    }

    return(inside_quote)
  }

  # Process each line: remove comments and empty lines
  cleaned_lines <- sapply(lines, function(line) {
    line <- remove_comments(line)
    line <- trimws(line)  # Remove leading and trailing whitespace
    if (nchar(line) > 0) {
      return(line)
    } else {
      return(NULL)
    }
  })

  # Remove NULL elements
  cleaned_lines <- cleaned_lines[!sapply(cleaned_lines, is.null)]

  # Properly indent the R file
  properly_indented_lines <- paste(capture.output(styler::style_text(cleaned_lines)), collapse = "\n")

  # Write the cleaned and indented lines back to a file
  writeLines(properly_indented_lines, output_file_path)

  cat("File cleaned and saved to", output_file_path, "\n")
}

# Example usage
# file_path <- ".testR"
# output_file_path <- ".cleaned_script.R"
# clean_r_file(file_path, output_file_path)

