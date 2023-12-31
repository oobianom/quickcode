% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/not.image.R
\name{is.image}
\alias{is.image}
\title{Is file name extension(s) an image}
\usage{
is.image(x)
}
\arguments{
\item{x}{vector entry}
}
\value{
a boolean value to indicate if entry is an image
}
\description{
Check if one or multiple file name entry is an image
}
\details{
This current function tests if the extension of the file name provided belongs to any of the image extensions listed below \cr
AI - Adobe Illustrator \cr
BMP - Bitmap Image \cr
CDR - Corel Draw Picture \cr
CGM - Computer Graphics Metafile \cr
CR2 - Canon Raw Version 2 \cr
CRW - Canon Raw \cr
CUR - Cursor Image \cr
DNG - Digital Negative \cr
EPS - Encapsulated PostScript \cr
FPX - FlashPix \cr
GIF - Graphics Interchange Format \cr
HEIC - High-Efficiency Image File Format \cr
HEIF - High-Efficiency Image File Format \cr
ICO - Icon Image \cr
IMG - GEM Raster Graphics \cr
JFIF - JPEG File Interchange Format \cr
JPEG - Joint Photographic Experts Group \cr
JPG - Joint Photographic Experts Group \cr
MAC - MacPaint Image \cr
NEF - Nikon Electronic Format \cr
ORF - Olympus Raw Format \cr
PCD - Photo CD \cr
PCX - Paintbrush Bitmap Image \cr
PNG - Portable Network Graphics \cr
PSD - Adobe Photoshop Document \cr
SR2 - Sony Raw Version 2 \cr
SVG - Scalable Vector Graphics \cr
TIF - Tagged Image File \cr
TIFF - Tagged Image File Format \cr
WebP - Web Picture Format \cr
WMF - Windows Metafile \cr
WPG - WordPerfect Graphics
}
\examples{
img.1 <- "fjk.jpg"
is.image(img.1)

img.0 <- "fjk.bbVG"
is.image(img.0)

img.2 <- "fjk.bmp"
is.image(img.2)

img.3 <- "fjk.SVG"
is.image(img.3)

# a vector of file names
v <- c("logo.png", "business process.pdf",
"front_cover.jpg", "intro.docx",
"financial_future.doc", "2022 buybacks.xlsx")

is.image(v)


# when the file name has no extension
# the function returns NA
v2 <- c("img2.jpg","northbound.xlsx","landimg",NA)
is.image(v2)

}
