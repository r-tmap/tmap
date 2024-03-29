% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_scale_.R
\name{tm_scale}
\alias{tm_scale}
\title{Scales: automatic scale}
\usage{
tm_scale(...)
}
\arguments{
\item{...}{arguments passed on to the applied scale function \verb{tm_scale_*()}}
}
\description{
Scales in tmap are configured by the family of functions with prefix \code{tm_scale}.
Such function should be used for the input of the \code{.scale} arguments in the
layer functions (e.g. \code{fill.scale} in \code{\link[=tm_polygons]{tm_polygons()}}). The function \code{tm_scale()}
is a scale that is set automatically given by the data type (factor, numeric, and integer)
and the visual variable. The tmap option \code{scales.var} contains information
which scale is applied when.
}
