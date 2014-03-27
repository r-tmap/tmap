\name{animateMaps}
\alias{animateMaps}
\title{Function to animate maps}
\usage{
animateMaps(expr, width = 1000, height = 1000, delay = 40,
  filename = "animation.gif")
}
\arguments{
  \item{expr}{R function call, e.g. with \code{choropleth}}

  \item{width}{width of the gif file (in pixels)}

  \item{height}{height of the gif file (in pixels)}

  \item{delay}{delay time between images}

  \item{filename}{filename of the video (should be a .gif
  or .mpg file)}
}
\description{
This function create a gif animation. The free tool
ImageMagick is required.
}
\keyword{animation}

