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
# load data
data(NLD_muni)

# convert to Rijksdriehoekstelsel
NLD_muni_RD <- rd2wgs84(NLD_muni)

# plot maps
geo_shape(NLD_muni) + geo_borders() + geo_theme(title="WGS84")
geo_shape(NLD_muni) + geo_borders() + geo_theme(title="RD")

}

