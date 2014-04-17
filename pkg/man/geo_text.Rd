\name{geo_text}
\alias{geo_text}
\title{Add text labels}
\usage{
geo_text(text, cex = 1, fontcolor = "black", fontface = "plain",
  fontfamily = "sans", bg.color = "#888888", bg.alpha = 100,
  cex.lowerbound = 0.2, print.tiny = FALSE, scale = 1)
}
\arguments{
  \item{text}{name of the variable in the shape object that
  contains the text labels}

  \item{cex}{relative size of the text labels. Eiter one
  number, a name of a numeric variable that is used to
  scale the sizes proportionally, or one of the following
  special cases \describe{ \item{\code{"AREA"}:}{text size
  is proportional to the squared root of the area size of
  the polygons.} \item{\code{"AREAx"}:}{text size is
  proportional to the x-th root of the area size of the
  polygons.}}}

  \item{fontcolor}{relative size of the text labels}

  \item{fontface}{font face of the text labels}

  \item{fontfamily}{font family of the text labels}

  \item{bg.color}{background color of the text labels}

  \item{bg.alpha}{number between 0 and 255 that specifies
  the transparancy of the text background (0 is totally
  transparent, 255 is solid background)}

  \item{cex.lowerbound}{lowerbound for \code{cex}. Needed
  to ignore the tiny labels in case \code{cex} is a
  variable.}

  \item{print.tiny}{boolean that determines if tiny labels
  (which size is smaller than \code{cex.lowerbound}) are
  print at size \code{cex.lowerbound}}

  \item{scale}{scalar needed in case cex is based}
}
\value{
\code{\link{geo-object}}
}
\description{
This layer adds text labels
}

