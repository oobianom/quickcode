#' Sort vector by length or file types of its content
#'
#' Sort the length or file types of the content of a vector
#'
#' @param vec a vector
#' @param asc A logical value indicating whether to sort in ascending (TRUE) or descending (FALSE) order. Default is TRUE.
#' @rdname sort-additives
#' @note
#' This function removes all NAs prior to sorting the vector
#' @return vector of items sorted by length
#'
#' @examples
#' # sort by length of content
#' x = c("acs","tt","jdssr","h","grab")
#' sort_length(vec = x) # ascending order of length
#' sort_length(vec = x, asc = FALSE) # descending order of length
#'
#'
#' @export
#'

sort_length <- function(vec, asc = TRUE) {
  # Remove NA values
  vec <- vec[not.na(vec)]

  # Compute lengths of elements
  lengths <- nchar(vec)

  # Sort indices based on lengths
  # Return sorted vector
  vec[order(lengths, decreasing = !asc)]
}




#' Sort Filenames by File Type
#'
#' @description
#' Takes a vector of file names and sorts them by their file extensions (file type)
#'
#' @param files A character vector containing file names to be sorted
#' @rdname sort-additives
#' @return A character vector of sorted file names
#'
#' @examples
#' files <- c("doc1.pdf",
#'   "image.jpg", "house.csv", "notes.txt",
#'   "patab","doc2.pdf", "data.csv", "pic.png","cotab")
#' sort_file_type(files)
#' sort_file_type(files, asc = FALSE)
#'
#' @export
sort_file_type <- function(files, asc = TRUE) {
  # Fetch the file extensions
  extensions <- tools::file_ext(files)
  # Sort based on asc parameter
  if(asc) {
    files[order(extensions)]
  } else {
    files[order(extensions, decreasing = TRUE)]
  }
}
