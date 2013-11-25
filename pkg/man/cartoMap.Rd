\name{cartoMap}
\alias{cartoMap}
\title{Create cartographic map}
\usage{
  cartoMap(shp, shp.col = NA, bubble = NULL,
    bubble.col = "red", plot.bubble.borders = TRUE,
    plot.bg = "white", plot.borders = "black", lwd = 1,
    xlim = c(0, 1), ylim = c(0, 1), units = "rel",
    labels = NULL, labels.cex = 1, shp2 = NULL,
    shp2.col = NA, plot.borders2 = "black", lwd2 = 2, ...)
}
\arguments{
  \item{shp}{a shape object}

  \item{shp.col}{vector of colors}

  \item{bubble}{vector of bubble sizes}

  \item{bubble.col}{bubble colour(s). Either a colour value
  or a vector of colours}

  \item{plot.bubble.borders}{plot the borders of the
  bubbles?}

  \item{lwd}{border line width}

  \item{plot.bg}{background color}

  \item{plot.borders}{color of the border. When set to
  \code{NA}, borders are not plot}

  \item{xlim}{two-valued vector that sets the x-range. See
  parameter \code{lim.units}.}

  \item{ylim}{two-valued vector that sets the y-range. See
  parameter \code{lim.units}.}

  \item{units}{units of \code{xlim} and \code{ylim}. Either
  "abs" for absolute values (e.g. RDS or wgs84
  coordinates), or "rel" for relative values between 0 and
  1.}

  \item{labels}{vector of text labels}

  \item{labels.cex}{font size of the labels}

  \item{shp2}{a shape object for additional borders}

  \item{shp2.col}{fill vector of fill colors of shp2}

  \item{plot.borders2}{color of the additional borders}

  \item{lwd2}{border line width for additional borders (use
  \code{lwd} for regular borders)}

  \item{...}{passed arguments}
}
\description{
  This function creates a cartographic map. It is used by
  the functions \code{\link{choropleth}} and
  \code{\link{bubbleMap}}.
}

