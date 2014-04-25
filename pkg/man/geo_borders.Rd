\name{geo_borders}
\alias{geo_borders}
\title{Draw polygon borders}
\usage{
geo_borders(col = "grey40", lwd = 1, lty = "solid")
}
\arguments{
  \item{col}{line color}

  \item{lwd}{line width (see
  \code{\link[graphics:par]{par}})}

  \item{lty}{line type (see
  \code{\link[graphics:par]{par}})}
}
\value{
\code{\link{geo-object}}
}
\description{
This layer defines the borders of the polygons. Color, line
width and line type can be set.
}
\examples{
data(Europe)

geo_shape(Europe) + geo_borders()

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

