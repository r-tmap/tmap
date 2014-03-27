\name{rd2wgs84}
\alias{rd2wgs84}
\title{Transforms rd(rijksdriekhoekstelsel)-based shape object to wgs84-based shape object.}
\usage{
rd2wgs84(shp)
}
\arguments{
  \item{shp}{object in rd-coordinates}
}
\value{
shp object in wgs84-coordinates
}
\description{
This function transforms a rd object to a wgs84 object,
that is used by Google Earth
}
\examples{
options(shp_dir=system.file("shapes", package="geoNL"))
shp.gm_rd <- getShape("gm", 2012)
shp.gm_wgs84 <- rd2wgs84(shp.gm_rd)


}

