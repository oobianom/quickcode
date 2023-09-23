#' Download random images from the web
#'
#' Retrieve n number of high-definition images by category from the web
#'
#' @param fp Storage directory
#' @param cat category of image to download
#' @param n number of images to download
#' @param size size of images to download
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
#' # download 1 image from the nature category
#' genImage(fp="~/",cat = "nature", n = 1)
#'
#'
#'
#' @export
#'
genImage <- function(fp = "~/images", cat = "nature", n = 1, w.px = 500, h.px = 500, ext = "jpg"){
  checksum.files <- c()
  while(n > 0) {
    temp.store <- paste0(tempfile(),".",ext)
    download.file(
      url = paste0("https://source.unsplash.com/random/",w.px,"x",h.px,"/?",cat,"&", i),
      destfile = temp.store, mode = 'wb')
    checksum <- tools::md5sum(temp.store)
    if(checksum %in% checksum.files){
      unlink(temp.store)
      plus(n)
    }else{
      vector_push(checksum.files,checksum)
      minus(n)
    }
    print(temp.store)
  }

}
