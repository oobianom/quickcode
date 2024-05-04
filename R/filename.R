#' Append date to filename
#'
#' Add today's date to the filename
#'
#' @param ... file name or path to concat
#' @param format time format e.g. \%d-\%b-\%Y , refer to \code{\link{date3to1}} for date formats
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
#' # Task 5
#' data("USArrests")
#' USArrests$fn = paste0(row.names(USArrests), ".txt")
#' head(fAddDate(USArrests$fn),10)
#' @export

fAddDate <- function(...,format = "%d-%b-%Y"){
  stopifnot(...length()>0)
  combine <- paste0(...)
  apply(data.frame(f=combine,e=tools::file_ext(combine)), 1, function(e){
    gsub(paste0("\\.",e['e'],"$"),
         paste0("_",format(Sys.time(), format),
                paste0(".",e['e'])),e['f'])
  })
}

