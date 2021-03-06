% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_dmdScheme_raw.R,
%   R/as_dmdScheme_raw.dmdSchemeData.R, R/as_dmdScheme_raw.dmdSchemeSet.R,
%   R/as_dmdScheme_raw.xml_document.R
\name{as_dmdScheme_raw}
\alias{as_dmdScheme_raw}
\alias{as_dmdScheme_raw.dmdSchemeData}
\alias{as_dmdScheme_raw.dmdSchemeSet}
\alias{as_dmdScheme_raw.xml_document}
\title{Generic function to convert the data stored in the object \code{x} into a new object of class \code{dmdScheme_raw...}}
\usage{
as_dmdScheme_raw(x, ...)

\method{as_dmdScheme_raw}{dmdSchemeData}(x, ...)

\method{as_dmdScheme_raw}{dmdSchemeSet}(x, ...)

\method{as_dmdScheme_raw}{xml_document}(x, useSchemeInXml = NULL, ...)
}
\arguments{
\item{x}{object to be converted}

\item{...}{additional arguments for methods}

\item{useSchemeInXml}{if \code{TRUE}, use scheme definition in xml and raise
an error if the xml does not contain a scheme definition. If False, use the
scheme definition from the corresponding installed package, even if the xml
contains a scheme definition. if \code{NULL} (the default), use the
definition in the xml if it contains a definition, if not use the
corresponding definition from the installed package.}
}
\value{
dmdScheme as object of class \code{dmdScheme_raw}
}
\description{
Generic function to convert the data stored in the object \code{x} into a new object of class \code{dmdScheme_raw...}
}
\examples{
as_dmdScheme_raw(dmdScheme(), keepData = TRUE)

}
