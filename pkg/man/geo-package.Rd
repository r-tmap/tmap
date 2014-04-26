\docType{package}
\name{geo-package}
\alias{geo-package}
\title{Statistical geographic maps}
\description{
\tabular{ll}{ Package: \tab geo\cr Type: \tab Package\cr
Version: \tab 0.4\cr Date: \tab 2014-04-25\cr License: \tab
GPL-3\cr LazyLoad: \tab yes\cr }
}
\details{
Tools for creating thematic cartographic maps. \itemize{
\item{ggplot2-style: \itemize{ \item Stack elements
intuitively (like ggplot) with great flexibility. The name
of each element starts with \code{geo_}. The foundation is
\code{geo_shape}, the simple drawing elements are
\code{\link{geo_borders}}, \code{\link{geo_fill}}, and
\code{\link{geo_borders}}, the statistical elements are
\code{\link{geo_choropleth}} and
\code{\link{geo_bubblemap}}, the theme element is
\code{\link{geo_theme}} and the element to control small
multiples is \code{\link{geo_grid}}. \item Create quick
maps (like qplot) with \code{\link{geo}}. All the elements
above can be called within one function call. }}
\item{Handy functions for working with shape files
(formally known as
\code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrames}})
\itemize{ \item read shape files directly or from a network
repository: \code{\link{get_shape}}; \item append data to
shape files: \code{\link{append_data}}; \item crop shape
files while retaining the data with
\code{\link{crop_shape}}. \item convert from RD
(rijksdriehoekstelsel) to wgs84 coordinates:
\code{\link{rd2wgs84}}; \item (not working yet) export to
kml format (for Google Earth): \code{shp2kml}; \item (not
working yet) interactive cartography: \code{igeo}. }}
\item{Example shape files of the World, Europe and the
Netherlands have been included}}
}
\author{
Martijn Tennekes \email{mtennekes@gmail.com}
}
\keyword{GIS}
\keyword{cartography}

