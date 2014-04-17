\name{geo_grid}
\alias{geo_grid}
\title{Small multiples grid}
\usage{
geo_grid(ncol = NULL, nrow = NULL, free.scales = FALSE,
  free.scales.choro = free.scales, free.scales.bubble.size = free.scales,
  free.scales.bubble.col = free.scales)
}
\arguments{
  \item{ncol}{number of columns of the small multiples
  grid}

  \item{nrow}{number of rows of the small multiples grid}

  \item{free.scales}{logical. Should all scales of the
  plotted data variables be free, i.e. independent of each
  other? Possible data variables are color from
  \code{\link{geo_choropleth}} and color and size from
  \code{\link{geo_bubblemap}}.}

  \item{free.scales.choro}{logical. Should the color scale
  for the choropleth be free?}

  \item{free.scales.bubble.size}{logical. Should the bubble
  size scale for the bubblemap be free?}

  \item{free.scales.bubble.col}{logical. Should the color
  scale for the bubblemap be free?}
}
\value{
\code{\link{geo-object}}
}
\description{
This layer specifies how the different plots are placed in
a grid. The number of rows and columns can be specified
here, as well as whether the scales are free (i.e.
independent of each other).
}

