\name{appendData}
\alias{appendData}
\title{Append data}
\usage{
appendData(shp, data, key.data = NULL, key.shp = NULL,
  ignore.duplicates = FALSE, fixed.order = is.null(key.data))
}
\arguments{
  \item{shp}{shape object}

  \item{data}{data.frame}

  \item{key.data}{variable name of \code{data} to be
  matched with \code{key.shp}. If not specified, and
  \code{fixed.order} is \code{FALSE}, the row names of
  \code{data} are taken.}

  \item{key.shp}{variable name of \code{shp} map data to be
  matched with \code{key.data}. If not specified, and
  \code{fixed.order} is \code{FALSE}, the polygon ID's are
  taken.}

  \item{ignore.duplicates}{should duplicated keys in
  \code{data} be ignored? (\code{FALSE} by default)}

  \item{fixed.order}{should the data be append in the same
  order as the shapes in \code{shp}?}
}
\value{
shape object with appended data
}
\description{
Append data.frame to shape object
}

