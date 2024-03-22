#' Append date to filename
#'
#' Add today's date to the filename
#'
#' @param ... file name or path to concat
#'
#' @details
#' The present function enables users to add the current date to the file name,
#' facilitating the straightforward saving of files with their respective dates.
#' It accepts different file paths and names as arguments, as demonstrated
#' in the example section. This functionality simplifies the process of associating
#' a file's creation date with its name, aiding users in recalling when a file was
#' saved. Moreover, it serves as a preventive measure against unintentional
#' overwriting of files created on different dates.
#'
#' @return file name with the current date added as suffix
#' @examples
#'
#' # Task 1
#' fAddDate("path1/","path2/filepre","filemid","fileend.png")
#'
#' # Task 2
#' fAddDate(c("path1/","path2/"),"filepre","filemid","fileend.png")
#'
#' # Task 3
#' fAddDate("one_file_name_fileend.pdf")
#'
#' # Task 4
#' fAddDate(c("path1/","path2/"),"filepre","filemid",c("fileend.png",".pdf"))
#'
#'
#' @export

fAddDate <- function(...){
  combine <- paste0(...)
  extt <- tools::file_ext(combine)
  for(ext in extt)
    combine <- gsub(paste0("\\.",ext,"$"),paste0("_",tolower(format(Sys.Date(),"%d-%b-%Y")),
                                                 paste0(".",ext)),combine)
  combine
}
