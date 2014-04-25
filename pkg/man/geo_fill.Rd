\name{geo_fill}
\alias{geo_fill}
\title{Fill polygons}
\usage{
geo_fill(col = "grey90")
}
\arguments{
  \item{col}{a single color value, or a vector of colors
  (specifying a color per polygon).}
}
\value{
\code{\link{geo-object}}
}
\description{
This layer defines the fill colors of the polygons. The
colors in this layer are directly specified. Use
\code{\link{geo_choropleth}} to map colors to a data
variable.
}
\examples{
data(World)

geo_shape(World) + geo_fill("green3") + geo_theme_World()

geo_shape(World, projection="merc", ylim=c(.2, 1)) + geo_fill(ifelse(World$iso_a3 \%in\% c("GRL", "AUS"), "gold", "gray75")) + geo_borders() + geo_theme("Mercator projection. Although used in Google Maps, it is discouraged for\\nstatistical purposes. In reality, Australia is 3 times larger than Greenland!", inner.margins=c(0,0,.15,0), title.cex=.6)

data(NLD_prov)
data(NLD_muni)

geo_shape(NLD_prov) + 
	geo_choropleth("name") + 
	geo_shape(NLD_muni) + 
	geo_borders() + 
	geo_shape(NLD_prov) + 
	geo_borders(lwd=2) +
	geo_text("name") +
	geo_theme_NLD("Provinces and municipalities", legend.profile="hide")
}
\seealso{
\code{\link{geo_choropleth}}
}

