% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractor.R
\name{extractor}
\alias{extractor}
\title{Create an Extractor Function}
\usage{
extractor(x, default = NA)
}
\arguments{
\item{x}{the list to create the extractor for.}

\item{default}{the default value to return in the case that an applied index turns out to be invalid.}
}
\value{
an extractor function for the list x.
}
\description{
Creates an extractor function for the given data input.
}
