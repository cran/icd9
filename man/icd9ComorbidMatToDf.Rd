% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{icd9ComorbidMatToDf}
\alias{icd9ComorbidMatToDf}
\title{convert matrix of comorbidities into data frame, preserving visitId
  information}
\usage{
icd9ComorbidMatToDf(x, visitId = "visitId",
  stringsAsFactors = getOption("stringsAsFactors"))
}
\arguments{
\item{x}{Matrix of comorbidities, with row and columns names defined}

\item{visitId}{Single character string with name for new column in output
data frame. Everywhere else, \code{visitId} describes the input data, but
here it is for output data.}

\item{stringsAsFactors}{whether the resulting data frame should have strings,
i.e. visitId converted to factor. Default is to follow the current session
option.}
}
\description{
convert matrix of comorbidities into data frame, preserving visitId
  information
}
\examples{
longdf <- data.frame(visitId = c("a", "b", "b", "c"),
    icd9 = c("441", "4424", "443", "441"))
mat <- icd9ComorbidElix(longdf)
class(mat)
typeof(mat)
rownames(mat)
df.out <- icd9ComorbidMatToDf(mat)
stopifnot(is.data.frame(df.out))
# output data frame has a factor for the visitId column
stopifnot(identical(rownames(mat), as.character(df.out$visitId)))
df.out[, 1:4]
}

