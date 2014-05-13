\name{geo_bubbles}
\alias{geo_bubbles}
\title{Draw bubbles}
\usage{
geo_bubbles(size = 1, col = "blueviolet", border = NA, scale = 1,
  xmod = 0, ymod = 0)
}
\arguments{
  \item{size}{relative sizes of the bubbles}

  \item{col}{a single color value, or a vector of colors
  (specifying a color per polygon).}

  \item{border}{color of the bubble borders. If \code{NA},
  no borders are drawn.}

  \item{scale}{scale multiplier to adjust the bubble sizes}

  \item{xmod}{}

  \item{ymod}{}
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
\examples{
data(Europe)

geo_shape(Europe) + geo_borders() + geo_bubbles(size=.75, col="red") + geo_theme("Countries")

geo_shape(Europe) + geo_borders() + geo_bubbles(size=.75, col=ifelse(Europe$name=="Isle of Man", "red", "blue")) + geo_theme("Find Isle of Man...")

}
\seealso{
\code{\link{geo_choropleth}}
}

