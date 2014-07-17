\name{crop_shape}
\alias{crop_shape}
\title{Crop shape file}
\usage{
crop_shape(shp, bbox = shp@bbox)
}
\arguments{
  \item{shp}{shape object. Either a
  \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}
  or a \code{\link[sp:SpatialPolygons]{SpatialPolygons}}}

  \item{bbox}{rectangle to crop the \code{shp} with. It is
  represented by a 2x2 matrix in which the x and y
  coordiantes are respectively row 1 and 2, and the minimum
  and maximum values are respectively column 1 and 2. By
  default the bounding box of \code{shp} is taken.}
}
\value{
a cropped shape object. Its bounding box is set to
\code{bb}. Data is retained in case \code{shp} is a
\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}.
A vector of matched ID's is stored as a attribute
\code{matchID}. This vector contains for each polygon in
the returned shape object the number of its orignal polygon
in \code{shp}.
}
\description{
Crops a shape file, i.e. SpatialPolygons(DataFrame), to a
rectangle which is by default its bounding box.
}

