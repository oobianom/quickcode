#' Addin snippet function to add header comment to a current opened file
#'
#' Shorthand to add header comment
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
############################################################################
##  Document Path: ", rstudioapi::getActiveDocumentContext()$path, "
##
##  Author:
##
##  Date: ", Sys.Date(), "
##
##  Title:
##
##  Description:
##
##  Required Files:
##
##  Exported Files:
##
##  R Version: ", version$version.string, "
##
#############################################################################
#############################################################################

  "))
}


#' Addin snippet function to custom section comment
#'
#' Shorthand to add section comment to current file
#'
#' @return Inserts section comment content for file
#'
#' @examples
#' if(interactive())
#' add.sect.comment()
#' @export
#'

add.sect.comment <- function() {
  insertInText(paste0("
#############################################################################
###  SECTION:
#############################################################################

Your code here

#############################################################################
  "))
}
