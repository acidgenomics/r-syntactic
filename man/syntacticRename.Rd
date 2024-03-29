% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename.R
\name{syntacticRename}
\alias{syntacticRename}
\title{Rename files and/or directories using a syntactic naming function}
\usage{
syntacticRename(
  path,
  recursive = FALSE,
  fun = c("kebabCase", "snakeCase", "camelCase", "upperCamelCase"),
  quiet = FALSE,
  dryRun = FALSE
)
}
\arguments{
\item{path}{\code{character}.
File and/or directory paths.}

\item{recursive}{\code{logical(1)}.
Should the function recurse into directories?}

\item{fun}{\code{character(1)}.
Function name.}

\item{quiet}{\code{logical(1)}.
Perform command quietly, suppressing messages.}

\item{dryRun}{\code{logical(1)}.
Return the proposed file path modifications without modification.}
}
\value{
\code{list}.
Named list containining \code{from} and \code{to} rename operations.
}
\description{
Rename files and/or directories using a syntactic naming function
}
\details{
Intelligently deals with a case-insensitive file system, if necessary.
This is very useful for macOS and Windows.

Our syntactic naming functions can result in changes that only differ in
case, which are problematic on case-insensitive mounts, and require movement
of the files into a temporary file name before the final rename.
}
\note{
Updated 2023-02-06.
}
\examples{
testdir <- AcidBase::tempdir2()
from <- file.path(testdir, c("helloWorld.txt", "fooBar.R"))
file.create(from)
print(basename(from))
output <- syntacticRename(from)
print(output)
AcidBase::unlink2(testdir)
}
