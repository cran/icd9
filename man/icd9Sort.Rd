% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ranges.R
\name{icd9Sort}
\alias{icd9Sort}
\alias{icd9SortDecimal}
\alias{icd9SortShort}
\title{sort short-form icd9 codes}
\usage{
icd9Sort(icd9, isShort = icd9GuessIsShort(icd9))

icd9SortShort(icd9Short)

icd9SortDecimal(icd9Decimal)
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

\item{icd9Short}{is a character vector of ICD-9 codes. If fewer than
five characters is given in a code, then the digits are greedily assigned
to hundreds, then tens, then units, before the decimal parts. E.g. "10"
becomes "010", not "0010"}

\item{icd9Decimal}{character vector of ICD-9 codes. If fewer than five
characters is given in a code, then the digits are greedily assigned to
hundreds, then tens, then units, before the decimal parts. E.g. "10"
becomes "010", not "0010"}
}
\value{
sorted vector of ICD-9 codes. Numeric, then E codes, then V codes.
}
\description{
Sorts lists of numeric only, V or E codes. Note that a simple
  numeric sort does not work for ICD-9 codes, since "162" > "1620", and also
  V codes precede E codes.
}
\details{
Implementation used fast built-in sort, then shuffles the E codes to
  the end.
}
\keyword{manip}

