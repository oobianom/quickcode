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
#' genRandImg(fp = tempdir(), n = 200)
#'
#' # download 5 random images with extension jif and return paths
#' genRandImg(fp = tempdir(), n = 5, ext = "jif", paths = TRUE)
#' }
#' @export
#'
genRandImg = function (fp, n = 1, w.px = 500, h.px = 500,
                       ext = "jpg", paths = FALSE, cat = NULL)
{
  if(not.null(cat)) message("Please note that the 'cat' category is now deprecated.")
  if (!dir.exists(fp))
    stop(paste0("The directory path declared in the 'fp' argument must exist."))
  if (n > 99) {
    warning("The value of n exceeds 99, so n was set to 99")
    n <- 99L
  }
  checksum.files <- c()
  downloaded.files <- c()
  while (n > 0) {
    file.store <- file.path(fp, paste0("img_", randString(1, 5), n, ".", ext))
    utils::download.file(url = paste0("https://picsum.photos/", w.px, "/", h.px), destfile = file.store, mode = "wb")
    Sys.sleep(1)
    checksum <- as.vector(tools::md5sum(file.store))
    if (checksum %in% checksum.files) {
      unlink(file.store)
    }
    else {
      vector_push(checksum.files, checksum)
      vector_push(downloaded.files, file.store)
      minus(n)
    }
  }
  message(paste0("Downloaded ", length(downloaded.files), " files to ",
                 fp))
  if (paths)
    gsub("\\\\", "/", downloaded.files)
}

