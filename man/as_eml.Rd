% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_eml.R, R/as_eml.default.R
\name{as_eml}
\alias{as_eml}
\alias{as_eml.default}
\title{Generic function to convert an object to an object which can be saved as EML}
\usage{
as_eml(x, ...)

\method{as_eml}{default}(x, ...)
}
\arguments{
\item{x}{object to be converted.}

\item{...}{additional arguments for methods}
}
\value{
A \code{list} object which can be converted to EML. It can be
  written to a file using \code{write_eml()}. The resulting eml file can be
  validated using \code{eml_validate()}.

  NB: This does only validate the EML format, and NOT the validation as
  defined in the scheme definition package!
}
\description{
Generic function to convert an object to an object which can be saved as EML
}
