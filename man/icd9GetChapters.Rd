% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explain.R
\name{icd9GetChapters}
\alias{icd9GetChapters}
\title{get ICD-9 Chapters from vector of ICD-9 codes}
\usage{
icd9GetChapters(icd9, isShort = icd9GuessIsShort(icd9), verbose = FALSE)
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

\item{verbose}{single logical value, defaults to \code{FALSE} in most functions.}
}
\description{
This runs quite slowly. Used too rarely to be worth optimizing
  now. This is used to build a master list of ICD-9 codes with their
  respective chapters, sub-chapters, etc.. The querying of the web page to
  get these is already done, and the results saved in the lists
  \code{icd9Chapters} etc which define ranges.
}
\keyword{internal}

