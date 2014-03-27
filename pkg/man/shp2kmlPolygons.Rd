\name{shp2kmlPolygons}
\alias{shp2kmlPolygons}
\title{Transform a shape object to kml file}
\usage{
shp2kmlPolygons(shp, cols, file = "./temp.kml")
}
\arguments{
  \item{shp}{shape object}

  \item{file}{filename of the kml file}
}
\description{
This function creates a kml file from a shape object. kml
files can be read with Google Earth.
}
\examples{
options(shp_dir=system.file("shapes", package="geoNL"))
shp.gm <- getShape("gm", 2012)
shp2kml(shp.gm, "test.kml")

}

