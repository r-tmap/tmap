\name{convert_shape_data}
\alias{convert_shape_data}
\title{Convert shape data}
\usage{
convert_shape_data(shp.from, shp.to, variables.from = NULL,
  variables.to = NULL)
}
\arguments{
  \item{shp.from}{the shape object to be converted. It
  sould contain data.}

  \item{shp.to}{the shape object to be converted into.}

  \item{variables.from}{names of the variable of
  \code{shp.from} to be converted. If missing, all numeric
  variables are taken.}

  \item{variables.to}{variable names to be used. Should be
  the same number of variable names as
  \code{variables.from}}
}
\value{
shape object \code{shp.to} with converted data from
\code{shp.from}
}
\description{
Convert data from one polygon shape to another. It uses an
intersection matrix, which stores the intersection ratios
of the two shape objects per polygon
}

