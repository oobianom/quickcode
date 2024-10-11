#' Download random images from the web
#'
#' Generate n number of high-definition images by category from the web
#'
#' @param fp CHARACTER. storage directory
#' @param cat CHARACTER. category of image to download
#' @param n NUMERIC. number of images to download, maximum n is 99
#' @param w.px NUMERIC. width in pixels
#' @param h.px NUMERIC. height in pixels
#' @param ext CHARACTER. file extension eg jpg, png
#' @param paths logical. whether to return paths
#'
#' @section Sources & References:
#' The random images are downloaded from https://picsum.photos
#'
#'
#' @section Use case:
#' This functionality is great for developers trying to obtain one or more images for use in displays/analysis
#' or simply to build robust web applications.
#'
#' @return downloaded image from a select image category
#'
#'
#' @examples
#' \donttest{
#' # download 2 image from the nature category
#' genRandImg(fp = tempdir(),n = 2)
#'
#' # download 4 random images with width = 600px and height 100px
#' genRandImg(
#'   fp = tempdir(),
#'   w.px = 600,
#'   h.px = 100)
#'
#' # download 10 random images with extension png
#' genRandImg(fp = tempdir(),n = 10, ext = "png")
#'
#'
#' # download 200 random images from category of school
#' # Note that maximum download is 99, so the function will only download 99
#' genRandImg(fp = tempdir(), n = 121)
#'
#' # download 5 random images with extension jif and return paths
#' genRandImg(fp = tempdir(), n = 5, ext = "jpg", paths = TRUE)
#' }
#' @export
#'
genRandImg = function (fp, n = 1, w.px = 500, h.px = 500,
                       ext = "jpg", paths = FALSE, cat = NULL)
{
  message("As of version 1.0.1, this function is now deprecated as the external URLs to aided download of images no longer work.")
}

