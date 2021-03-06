% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upgrade_old_files.R
\name{upgrade_old_files}
\alias{upgrade_old_files}
\title{Convert older scheme versions of files to newer newer versions}
\usage{
upgrade_old_files(file, to = scheme_active()$version)
}
\arguments{
\item{file}{file name of \code{xlsx} or of \code{xml} file containing scheme
metadata or structure}

\item{to}{version to upgrade to. Any version supported is possible, downgrade
is not supported.}
}
\value{
if a conversion has been done, file name of upgraded spreadsheet
  (\code{BASENAME(x).to.EXTENSION(x)} where \code{x} is the original file
  name and \code{to} is the new version), otherwise \code{NULL}.
}
\description{
Only the newest versions of \code{xlsx} and \code{xml} files can be processed
by this package. To gurantee, this function provides a mechanism to convert
older versions of \code{xlsx} and \code{xml} files to newer versions.
}
\examples{
\dontrun{
upgrade("dmdScheme.xlsx")
upgrade("dmdScheme.xml")
}
}
