\name{saveMap}
\alias{saveMap}
\title{Save a thematic map}
\usage{
saveMap(e, file, width = 7, height = 7, res = 200, ...)
}
\arguments{
  \item{e}{expression that generates the plot (note: if a
  pdf file is specificied, and multiple plots are
  generated, for instance by chosing multiple variables
  while keeping \code{mfcol=mfrow=1}, the plots are created
  on different pages)}

  \item{file}{filename, including the extension (one of
  pdf, png, jpg, bmp, and tiff)}

  \item{width}{width in inches}

  \item{height}{height in inches}

  \item{res}{the resolution of the plot, i.e. dpi (dots per
  inch). Only applicable for bitmap formats (all but pdf)}

  \item{...}{arguments passed on to specific device (e.g.
  pdf)}
}
\description{
This function saves a thematic map (from
\code{\link{cartoMap}}, \code{\link{cartoMap}} or
\code{\link{cartoMap}}) to pdf, png, jpg, ...
}

