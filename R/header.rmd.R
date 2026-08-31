#' Snippet function to add header to a current Rmd opened file
#'
#' Shorthand to add Rmd header
#'
#' @return Inserts header content for Rmd file
#'
#' @examples
#' if(interactive())
#' header.rmd()
#' @export
#'

header.rmd <- function() {
  insertInText(paste0("
<!---

Document Path: ", rstudioapi::getActiveDocumentContext()$path, "

Author:

Date: ", Sys.Date(), "

Title:

Description:

Required Files:

Exported Files:

R Version: ", version$version.string, "


--->"))
}
