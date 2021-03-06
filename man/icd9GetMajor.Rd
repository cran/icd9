% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{icd9GetMajor}
\alias{icd9GetMajor}
\alias{icd9GetMajorShort}
\title{Get major (three-digit) part of ICD-9 codes}
\usage{
icd9GetMajor(icd9, isShort)

icd9GetMajorShort(icd9Short)
}
\arguments{
\item{icd9}{is
a character vector or factor of ICD-9 codes. If fewer than five characters
is given in a code, then the digits are greedily assigned to hundreds, then
tens, then units, before the decimal parts. E.g. "10" becomes "010", not
"0010".}

\item{isShort}{single logical value which determines whether the ICD-9 code
provided is in short (TRUE) or decimal (FALSE) form. Where reasonable, this
is guessed from the input data.}
}
\description{
This is reasonably fast, but calculates all the minors, then throws away the result.
}
\keyword{internal}
\keyword{manip}

