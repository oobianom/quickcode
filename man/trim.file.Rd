% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim_file.R
\name{trim.file}
\alias{trim.file}
\title{Remove Empty Lines from a File}
\usage{
trim.file(file_path, resave = TRUE)
}
\arguments{
\item{file_path}{A character string specifying the path to the text file.}

\item{resave}{A logical value indicating if the file content should be resaved or returned}
}
\value{
NULL This function modifies the file in place and does not return a value.
}
\description{
This function reads a text file, removes all empty lines, and saves the
modified content back to the same file.
}
\examples{
\donttest{
if(interactive()){
# Remove empty lines from a file
trim.file("path/to/your/file.txt")
}
}
}
