\name{geo_facets}
\alias{geo_facets}
\title{Small multiples grid}
\usage{
geo_facets(by = NULL, ncol = NULL, nrow = NULL,
  free.scales = is.null(by), free.scales.fill = free.scales,
  free.scales.bubble.size = free.scales,
  free.scales.bubble.col = free.scales, free.scales.line.col = free.scales,
  free.scales.line.lwd = free.scales)
}
\arguments{
  \item{by}{data variable name by which the data is split}

  \item{ncol}{number of columns of the small multiples
  grid}

  \item{nrow}{number of rows of the small multiples grid}

  \item{free.scales}{logical. Should all scales of the
  plotted data variables be free, i.e. independent of each
  other? Possible data variables are color from
  \code{\link{geo_fill}}, color and size from
  \code{\link{geo_bubbles}} and line color from
  \code{\link{geo_lines}}.}

  \item{free.scales.fill}{logical. Should the color scale
  for the choropleth be free?}

  \item{free.scales.bubble.size}{logical. Should the bubble
  size scale for the bubblemap be free?}

  \item{free.scales.bubble.col}{logical. Should the color
  scale for the bubblemap be free?}

  \item{free.scales.line.col}{Should the line color scale
  be free?}

  \item{free.scales.line.lwd}{Should the line width scale
  be free?}
}
\value{
\code{\link{geo-element}}
}
\description{
This layer specifies how small multiples are placed in a
grid. Either the argument \code{by} should be specified,
i.e. the name of a variable by which the data is grouped,
or multiple variable names sould be provided with
\code{\link{geo_fill}}, \code{\link{geo_lines}}, or
\code{\link{geo_bubbles}}. In this function, the number of
rows and columns can be specified, as well as whether the
scales are free (i.e. independent of each other).
}
\examples{
## World example
data(World)
geo_shape(World) +
	geo_fill(c("green", "blue")) +
	geo_theme(c("A green world", "A blue world"))

## Europe example
data(Europe)
geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "pop_est_dens")) +
	geo_theme_Europe(scale=2)



## Netherlands example
data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_muni) +
	geo_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"), convert2density=TRUE, style="kmeans") +
	geo_shape(NLD_prov) +
	geo_borders() +
	geo_facets(free.scales=FALSE) +
	geo_theme(c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44", "Population 45 to 64", "Population 65 and older"), scale=4)
}

