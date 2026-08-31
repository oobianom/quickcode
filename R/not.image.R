#' File name extension(s) is Not an image
#'
#' Check if one or multiple file name entry is not an image
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is not an image
#'
#' @details
#' This current function tests if the extension of the file name provided does NOT belongs to any of the image extensions listed below \cr
#' AI - Adobe Illustrator \cr
#' BMP - Bitmap Image \cr
#' CDR - Corel Draw Picture \cr
#' CGM - Computer Graphics Metafile \cr
#' CR2 - Canon Raw Version 2 \cr
#' CRW - Canon Raw \cr
#' CUR - Cursor Image \cr
#' DNG - Digital Negative \cr
#' EPS - Encapsulated PostScript \cr
#' FPX - FlashPix \cr
#' GIF - Graphics Interchange Format \cr
#' HEIC - High-Efficiency Image File Format \cr
#' HEIF - High-Efficiency Image File Format \cr
#' ICO - Icon Image \cr
#' IMG - GEM Raster Graphics \cr
#' JFIF - JPEG File Interchange Format \cr
#' JPEG - Joint Photographic Experts Group \cr
#' JPG - Joint Photographic Experts Group \cr
#' MAC - MacPaint Image \cr
#' NEF - Nikon Electronic Format \cr
#' ORF - Olympus Raw Format \cr
#' PCD - Photo CD \cr
#' PCX - Paintbrush Bitmap Image \cr
#' PNG - Portable Network Graphics \cr
#' PSD - Adobe Photoshop Document \cr
#' SR2 - Sony Raw Version 2 \cr
#' SVG - Scalable Vector Graphics \cr
#' TIF - Tagged Image File \cr
#' TIFF - Tagged Image File Format \cr
#' WebP - Web Picture Format \cr
#' WMF - Windows Metafile \cr
#' WPG - WordPerfect Graphics
#'
#'
#' @examples
#' img.1 <- "fjk.jpg"
#' not.image(img.1)
#'
#' img.2 <- "fjk.bmp"
#' not.image(img.2)
#'
#' img.3 <- "fjk.SVG"
#' not.image(img.3)
#'
#' # a vector of file names
#' v <- c("logo.png", "business process.pdf",
#' "front_cover.jpg", "intro.docx",
#' "financial_future.doc", "2022 buybacks.xlsx")
#' not.image(v)
#'
#' # when the file name has no extension
#' # the function returns NA
#' v2 <- c("img2.jpg",NA,"northbound.xlsx","landimg")
#' not.image(v2)
#' @export

not.image <- function(x)!{
  is.image(x)
}




#' Is file name extension(s) an image
#'
#' Check if one or multiple file name entry is an image
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is an image
#'
#' @details
#' This current function tests if the extension of the file name provided belongs to any of the image extensions listed below \cr
#' AI - Adobe Illustrator \cr
#' BMP - Bitmap Image \cr
#' CDR - Corel Draw Picture \cr
#' CGM - Computer Graphics Metafile \cr
#' CR2 - Canon Raw Version 2 \cr
#' CRW - Canon Raw \cr
#' CUR - Cursor Image \cr
#' DNG - Digital Negative \cr
#' EPS - Encapsulated PostScript \cr
#' FPX - FlashPix \cr
#' GIF - Graphics Interchange Format \cr
#' HEIC - High-Efficiency Image File Format \cr
#' HEIF - High-Efficiency Image File Format \cr
#' ICO - Icon Image \cr
#' IMG - GEM Raster Graphics \cr
#' JFIF - JPEG File Interchange Format \cr
#' JPEG - Joint Photographic Experts Group \cr
#' JPG - Joint Photographic Experts Group \cr
#' MAC - MacPaint Image \cr
#' NEF - Nikon Electronic Format \cr
#' ORF - Olympus Raw Format \cr
#' PCD - Photo CD \cr
#' PCX - Paintbrush Bitmap Image \cr
#' PNG - Portable Network Graphics \cr
#' PSD - Adobe Photoshop Document \cr
#' SR2 - Sony Raw Version 2 \cr
#' SVG - Scalable Vector Graphics \cr
#' TIF - Tagged Image File \cr
#' TIFF - Tagged Image File Format \cr
#' WebP - Web Picture Format \cr
#' WMF - Windows Metafile \cr
#' WPG - WordPerfect Graphics
#'
#'
#' @examples
#' img.1 <- "fjk.jpg"
#' is.image(img.1)
#'
#' img.0 <- "fjk.bbVG"
#' is.image(img.0)
#'
#' img.2 <- "fjk.bmp"
#' is.image(img.2)
#'
#' img.3 <- "fjk.SVG"
#' is.image(img.3)
#'
#' # a vector of file names
#' v <- c("logo.png", "business process.pdf",
#' "front_cover.jpg", "intro.docx",
#' "financial_future.doc", "2022 buybacks.xlsx")
#'
#' is.image(v)
#'
#'
#' # when the file name has no extension
#' # the function returns NA
#' v2 <- c("img2.jpg","northbound.xlsx","landimg",NA)
#' is.image(v2)
#'
#' @export

is.image <- function(x){
  exts <- tools::file_ext(tolower(x))
  res <- unlist(lapply(exts, function(ext) ext %in% imageext  ))
  res[c(which(!nchar(exts) | is.na(exts)))] = NA
  res
}


# Future goal of the is.image is to be able to read into files and determine if they are image files !! More development to be done
