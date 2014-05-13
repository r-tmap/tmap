\name{geo}
\alias{geo}
\title{Quick map plot}
\usage{
geo(shp, choro.fill = NULL, bubble.size = NULL, bubble.col = NULL,
  borders = "grey40", fill = "grey90", text = NULL, theme = NULL, ...)
}
\arguments{
  \item{shp}{shape object. For \code{\link{geo_choropleth}}
  and \code{\link{geo_bubblemap}}, a
  \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}
  or a
  \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}}
  is requied. \code{\link[sp:SpatialPoints]{SpatialPoints}}
  and
  \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}}
  are only used for \code{\link{geo_bubblemap}} and
  \code{\link{geo_bubbles}}.}

  \item{choro.fill}{name of the data variable in \code{shp}
  for the choropleth. If not specified, no choropleth is
  drawn.}

  \item{bubble.size}{name of the data variable in
  \code{shp} for the bubblemap that specifies the sizes of
  the bubbles. If neither \code{bubble.size} nor
  \code{bubble.col} is specified, no bubblemap is drawn.}

  \item{bubble.col}{name of the data variable in \code{shp}
  for the bubblemap that specifies the colors of the
  bubbles. If neither \code{bubble.size} nor
  \code{bubble.col} is specified, no bubblemap is drawn.}

  \item{borders}{color of the polygon borders. Use
  \code{NA} to omit the borders.}

  \item{fill}{specifies the fill colors in case
  \code{choro.fill} is not specified}

  \item{text}{name of a variable in \code{shp} that contain
  text labels}

  \item{theme}{one of "World", "Europe", or "NLD"}

  \item{...}{parameters passed on to the \code{geo_*}
  functions.}
}
\value{
\code{\link{geo-object}}
}
\description{
This function is a convenient wrapper for drawing quick
cartographic maps.
}
\examples{
data(Europe)

# just the map
geo(Europe)

# choropleth
g <- geo(Europe, choro.fill="gdp_cap_est", theme="Europe", style="kmeans", title="Estimated GDP per capita")


# bubblemap
geo(Europe, bubble.size="pop_est", bubble.col="part", scale=2, theme="Europe")


# World maps
data(World)
geo(World, choro.fill="pop_est_dens", theme="World", style="kmeans", title="Population per km")
}

