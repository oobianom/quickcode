# to-do v1.0
# function to track function usage
# type3 <-function(x)type1(x)
# type1 <- function(x){
#   mean(x)
#   sd(x)
#   track_func
# }

#'
track_func <- function(apiId, output.dest = "output_tracking.csv"){
  getCall<-as.character(sys.calls()[[length(sys.calls())-1]])
  getFuncName <- strsplit(getCall,"\\(")[[1]][1]
  print(getFuncName)
  ls("package:quickcode")
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
    message(line)
    line <- remove_content_in_quotes(line)
    print(line)
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
remove_comments <- function(line){
  stopifnot(length(line) == 1)
  comment_index <- regexpr("#", line)
  if(comment_index == -1) line else gsub("#.*$", "", line)
}


# Function to numb internal comments
hide_int_cmts <- function(string) {
  string <- gsub('("[^"]*)#+([^"]*)"', '\\10x5&9%80x\\2"', string, perl = TRUE)
  string <- gsub("('[^']*)#+([^']*)'", '\\10x5&9%80x\\2\'', string, perl = TRUE)
  string
}


#' @rdname comments
#' @param output_file file path to write the output of the new file
#' @export
#' @examples
#' \dontrun{
#' # Example usage
#' file_path <- ".testR"
#' output_file_path <- ".cleaned_script.R"
#' clean_file(file_path, output_file_path)
#' }
#'
#'
clean_file <- function(file, output_file) {
  # Read the file line by line
  lines <- readLines(file)

  # Process each line: remove comments and empty lines
  cleaned_lines <- sapply(lines, function(line) {
    line <- hide_int_cmts(line)
    line <- gsub(master_file_clean_sep ,"#",remove_comments(line))
    line <- trimws(line)  # Remove leading and trailing whitespace
    if (nchar(line) > 0) {
      return(line)
    } else {
      return(NULL)
    }
  })
  res <- as.character(cleaned_lines[!sapply(cleaned_lines, is.null)])
  if(missing(output_file)) res else writeLines(res, output_file)
}


#' Get all defined functions within a file
#'
#' @rdname comments
#' @export
get_function_def <- function(file) {
  code <- clean_file(file)
  envi = new.env()
  eval(parse(text = code), envir = envi)
  ls(envir = envi)
}
