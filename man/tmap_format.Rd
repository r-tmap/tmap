% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmap_format.R
\name{tmap_format}
\alias{tmap_format}
\alias{tmap_format_add}
\title{Get or add format options}
\usage{
tmap_format(format)

tmap_format_add(..., name)
}
\arguments{
\item{format}{name of the format. Run \code{tmap_format()} to see the choices.}

\item{...}{options from \code{\link{tm_layout}} or \code{\link{tm_view}}. Can also be a list of those options.}

\item{name}{name of the new format.}
}
\value{
the function \code{tmap_format()} returns the names of the available formats. When \code{format} is defined, it returns the option list corresponding the that format.
}
\description{
Format options are tmap options that are shape dependend. With \code{tmap_format()} the predefined formats can be retrieved. The values for a specific format can be retrieved with \code{tmap_format(format)}, where format is the name of the format. The function \code{tmap_format_add} is used to add a format.
}
\examples{
# available formats
tmap_format()

# create option list to be used as a new format
World_small <- tmap_format("World")
World_small$scale <- 2

# add format
tmap_format_add(World_small, name = "World_small")

# observe that World_small is successfully added:
tmap_format()

data(World)

qtm(World, fill="HPI", format="World_small")
}
\seealso{
\code{\link{tm_layout}} for predefined styles, \code{\link{tmap_style_catalogue}} to create a style catelogue of all available styles, and \code{\link{tmap_options}} for tmap options.

\code{\link{tmap_options}} for tmap options
}