#' File name is Not an image
#'
#' Check if a file name entry is not an image
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is not an image
#' @examples
#' img.1 = "fjk.jpg"
#' not.image(img.1)
#'
#' img.2 = "fjk.bmp"
#' not.image(img.2)
#'
#' img.3 = "fjk.SVG"
#' not.image(img.3)
#'
#' @export

not.image <- function(x) !is.image(x)




#' Is file extension an image
#'
#' Check if a file name entry is an image
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is an image
#' @examples
#' img.1 = "fjk.jpg"
#' is.image(img.1)
#'
#' img.0 = "fjk.bbVG"
#' is.image(img.0)
#'
#' img.2 = "fjk.bmp"
#' is.image(img.2)
#'
#' img.3 = "fjk.SVG"
#' is.image(img.3)
#'
#' @export

is.image <- function(x){
  if(tools::file_ext(tolower(x)) %in% c('jpeg','jpg','jfif','png','gif','tiff','tif','bmp','svg','ico','psd','ai','eps','webp','heic','heif','nef','cr2','orf','dng','crw','sr2')) TRUE else FALSE
}


# Future goal of the is.image is to be able to read into files and determine if they are image files !! More development to be done
