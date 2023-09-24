#' Download random images from the web
#'
#' Generate n number of high-definition images by category from the web
#'
#' @param fp storage directory
#' @param cat category of image to download
#' @param n number of images to download, maximum n is 99
#' @param w.px width in pixels
#' @param h.px height in pixels
#' @param ext file extension eg jpg, png
#'
#' @section SOURCE:
#' The random images are downloaded from www.unsplash.com
#'
#' @section USE CASE:
#' This functionality is great for R developers trying to obtain one or more images for using in analysis or simply for building robust
#' web applications.
#'
#' @return downloaded image from a select image category
#'
#'
#' @examples
#' # download 2 image from the nature category
#' genRandImg(fp = tempdir(),cat = "nature", n = 2)
#'
#'
#'
#' @export
#'
genRandImg <- function(fp, cat, n = 1, w.px = 500, h.px = 500, ext = "jpg") {
  # check that n does not exceed 99
  if(n > 99){
    warning("The value of n exceeds 99, so n was set to 99")
    n <- 99L
  }
  # check sum and file names
  checksum.files <- c()
  downloaded.files <- c()

  # download files to temp directory
  while (n > 0) {
    file.store <- file.path(fp,paste0(tolower(cat),"_",randString(1,5),"_",n,".",ext))
    download.file(
      url = paste0("https://source.unsplash.com/random/", w.px, "x", h.px, "/?", cat, "&", n),
      destfile = file.store, mode = "wb"
    )

    # make sure there is no duplication
    checksum <- as.vector(tools::md5sum(file.store))
    if (checksum %in% checksum.files) {
      unlink(file.store)
    } else {
      vector_push(checksum.files, checksum)
      vector_push(downloaded.files, file.store)
      minus(n)
    }
  }
  message(paste0("Downloaded ",length(downloaded.files)," files to ",fp))
}


