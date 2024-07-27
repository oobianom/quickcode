# to-do v1.0
# function to track function usage
type3 <-function(x)type1(x)
type1 <- function(x){
  mean(x)
  sd(x)
  tracker()
}

tracker <- function(apiId){
  getCall<-as.character(sys.calls()[[length(sys.calls())-1]])
  getFuncName <- strsplit(getCall,"\\(")[[1]][1]
  print(getFuncName)
  ls("package:quickcode")
}




extract_comments <- function(file_path) {
  # Read the file line by line
  lines <- readLines(file_path)

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
