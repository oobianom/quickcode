% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv.gm.R
\name{geo.cv}
\alias{geo.cv}
\title{Calculate geometric coefficient of variation and round}
\usage{
geo.cv(num, na.rm = TRUE, neg.rm = TRUE, pct = TRUE, round = 2)
}
\arguments{
\item{num}{vector of numbers}

\item{na.rm}{remove NAs from the vector}

\item{neg.rm}{remove negative values from the vector}

\item{pct}{TRUE or FALSE. should result be in percent}

\item{round}{round result to decimal place}
}
\value{
the geometric cv of a set of numbers
}
\description{
Calculate the coefficient of variation and round
}
\examples{
#simulate numbers using a fixed seed
num1 <- number(n = 1115,max.digits = 4, seed = 10)

#get geometric CV, represent as percent and round to 2 decimal places
geo.cv(num1,round = 2) # result: 60.61\%

#or round to 3 decimal places
geo.cv(num1,round = 3) # result: 60.609\%

#by default, the above examples return a CV\%
#if you do not want the result as percentage, specify "pct"
geo.cv(num1,pct = FALSE) # result: 0.61

}
