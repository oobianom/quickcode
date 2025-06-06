% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aided_chatgpt.R
\name{mode.calc}
\alias{mode.calc}
\title{Calculate the Mode of a Numeric or Character Vector}
\usage{
mode.calc(x)
}
\arguments{
\item{x}{A numeric or character vector for which the mode is to be calculated.}
}
\value{
The mode(s) of the input vector. If multiple values have the same highest frequency, all modes are returned.
        Returns `NA` if the input vector is empty.
}
\description{
This function calculates the mode (most frequently occurring value(s)) of a numeric or character vector.
}
\examples{
# Example with a numeric vector
numeric_vector <- c(1, 2, 2, 3, 3, 3, 4, 5)
mode.calc(numeric_vector)

# Example with a character vector
character_vector <- c("apple", "banana", "apple", "orange", "banana", "banana")
mode.calc(character_vector)

}
