\name{geo_bubbles}
\alias{geo_bubbles}
\title{Draw bubbles}
\usage{
geo_bubbles(size = 1, col = "red", border = NA, scale = 1)
}
\arguments{
  \item{size}{relative sizes of the bubbles}

  \item{col}{a single color value, or a vector of colors
  (specifying a color per polygon).}

  \item{border}{color of the bubble borders. If \code{NA},
  no borders are drawn.}

  \item{scale}{scale multiplier to adjust the bubble sizes}
}
\value{
\code{\link{geo-object}}
}
\description{
This layer speficies the drawing of bubbles. The colors and
sizes of the bubbles are directly specified in this layer.
Use \code{\link{geo_bubblemap}} to map bubbles colors
and/or sizes to data.
}
\seealso{
\code{\link{geo_choropleth}}
}

