\name{aggregateShapeData}
\alias{aggregateShapeData}
\title{Aggregete shape data}
\usage{
aggregateShapeData(shp.from, shp.to, variables = NULL)
}
\arguments{
  \item{shp.from}{the (detailed) shape object to be
  aggregated.}

  \item{shp.to}{the (global) shape object that defines the
  aggregates}

  \item{variables}{variable names of \code{shp.to}. If
  missing, all numeric variables are taken.}
}
\description{
Aggregates numeric data from a detailed shape object to a
global shape object. It uses an intersection matrix, which
stores the intersection ratios of each detailed polygon and
each global polygon.
}

