\name{getAreas}
\alias{getAreas}
\title{Determine area sizes of the shapes.}
\usage{
  getAreas(shp)
}
\arguments{
  \item{shp}{shape object, i.e. a
  SpatialPolygons(DataFrame)}
}
\value{
  numeric vector of area sizes
}
\description{
  This function calculates the area sizes of the shapes.
  This is done by summing all polygons area sizes of the
  shape object that are not a hole, and substracting the
  area sized of shape objects that are holes.
}

