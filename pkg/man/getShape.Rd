\name{getShape}
\alias{getShape}
\title{Get shape file}
\usage{
getShape(id, year = NULL, dir = getOption("shp_dir"))
}
\arguments{
  \item{id}{either a region label (see \code{regions}) or a
  shape file name (including directory).}

  \item{year}{when \code{id} is specified as a region, then
  year must also be specified. Together, they define the
  shape file "region_year.shp".}

  \item{dir}{the directory of the shape files. Only
  applicable when \code{id} is specifies as region label.
  It should have a subdirectory for each year, and the
  shape files should be names according to the region label
  (see \code{regions}), followed by an underscore and the
  year.}
}
\value{
shape object
}
\description{
This function loads a shape file, either directly, or from
a (network) directory with shape files.
}
\examples{
options(shp_dir=system.file("shapes", package="geoNL"))
shp <- getShape("gm", 2012)


}

