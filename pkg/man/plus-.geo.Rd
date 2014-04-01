\name{+.geo}
\alias{+.geo}
\title{Stacking of geo layers}
\usage{
\method{+}{geo}(e1, e2)
}
\arguments{
  \item{e1}{first geo layer}

  \item{e2}{second geo layer}
}
\description{
The plus operator allows you to stack geo layers (created
by any geo_* function). Always start with geo_shape to
specify the shape object (i.e. the
SpatialPolygonsDataframe). If multile shape objects are
used, start each group of layers with geo_shape to specify
the shape object of that group.
}

