\name{appendData}
\alias{appendData}
\title{Append data}
\usage{
  appendData(shp, data, key.data = NULL, key.shp = NULL,
    ignore.duplicates = FALSE)
}
\arguments{
  \item{shp}{shape object}

  \item{data}{data.frame}

  \item{key.data}{variable name of \code{data} to be
  matched with \code{key.shp}}

  \item{key.shp}{variable name of \code{shp} map data to be
  matched with \code{key.data}}

  \item{ignore.duplicates}{should duplicated keys in
  \code{data} be ignored? (\code{FALSE} by default)}
}
\value{
  shape object with appended data
}
\description{
  Append data.frame to shape object
}

