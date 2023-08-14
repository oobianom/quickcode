#' Snippet function to add header to a current opened file
#'
#' Shorthand to add header
#'
#' @return Inserts header content for file
#'
#' @examples
#' if(interactive())
#' add.header()
#' @export
#'

add.header <- function() {
  insertInText(paste0("
############################################################################
#  Document Path: ", rstudioapi::getActiveDocumentContext()$path, "
#
#  Author:
#
#  Date: ", Sys.Date(), "
#
#  Title:
#
#  Description:
#
#  Required Files:
#
#  Exported Files:
#
#  R Version: ", version$version.string, "
#
#############################################################################

  "))
}
