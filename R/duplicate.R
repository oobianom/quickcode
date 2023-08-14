#' Duplicate a file and global replace
#'
#' Shorthand to return a re-sample number of rows in a data frame by unique column
#'
#' @param file data frame to re-sample
#' @param new.name column to uniquely re-sample
#' @param pattern number of rows to return
#' @param replacement unique numeric value for reproducibility
#' @param open description
#' @return data frame containing re-sampled rows from an original data frame
#'
#' @examples
#' \donttest{
#' duplicate('./file.R','file2.R','text1','replacement1')
#' }
#' @export
#'
duplicate <- function(file, new.name,pattern, replacement,open = TRUE){
  #exit if the file does not exist
  if(!file.exists(file)) stop("The file you are trying to duplicate does not exist.")

  #get initial file
  .file.1 <- readLines(file)

  #substitute text
  if(not.null(pattern) & not.null(replacement)){
    for(.i in 1:length(pattern)){
      .file.1 <- gsub(pattern[.i],replacement[.i],.file.1)
    }
  }

  #write to new file
  writeLines(.file.1,new.name)

  message(sprintf("The new file '%s' was successfully created!",new.name))

  if(open) rstudioapi::navigateToFile(new.name)
}


#' Prompt guided duplication if files
#'
#' AI like duplication and editing of files
#'
#' @param file file to duplicate
#' @param new.name OPTIONAL.name of new file
#' @param open open file after duplication
#'
#' @return duplicated files with edited texts
#'
#' @examples
#' \donttest{
#' ai.duplicate('./file.R','file2.R')
#' }
#'
#'
#' @export
#'
ai.duplicate <- function(file = NULL, new.name = NULL , open = TRUE) {
  #declare initial pattern and replacement
  init(pattern, replacement)

  if (is.null(file)) {
    file <- readline(prompt = "What file are you trying to duplicate?")
  }

  if(!file.exists(file)) stop("The file you are trying to duplicate does not exist.")

  if (is.null(new.name)) {
    new.name <- readline(prompt = "What is the new file name?")
  }

  #strings to replace
  repeat{
    vector_push(pattern,readline(prompt = "What string would you like to replace?"))
    vector_push(replacement,readline(prompt = "What will you like to replace it with?"))

    #check if more replacements are needed
    if(!as.logical(toupper(readline(prompt = "Want to replace more (T = Yes, F = No) ?"))))
      break
  }
  #duplicate file with entered parameters
  duplicate(file, new.name, pattern, replacement, open = open)
  invisible(file)
}

