% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/autopadZeros-methods.R
\name{autopadZeros}
\alias{autopadZeros}
\alias{autopadZeros,character-method}
\alias{autopadZeros,integer-method}
\title{Autopad zeros}
\usage{
autopadZeros(object, ...)

\S4method{autopadZeros}{character}(object)

\S4method{autopadZeros}{integer}(object)
}
\arguments{
\item{object}{Object.}

\item{...}{Additional arguments.}
}
\value{
\code{character}.
}
\description{
Autopad zeros
}
\note{
Updated 2023-09-21.
}
\examples{
## integer ====
autopadZeros(c(1L, 10L, 100L))

## character ====
## Left side.
autopadZeros(c("1-EV-DMSO", "10-EV-DMSO", "2-EV-DMSO-B"))
## Right side.
autopadZeros(c("A1", "B10", "C100"))
}
\seealso{
\itemize{
\item \code{\link[stringr:str_pad]{stringr::str_pad()}}.
}
}
