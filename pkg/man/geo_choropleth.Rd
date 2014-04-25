\name{geo_choropleth}
\alias{geo_choropleth}
\title{Draw choropleth}
\usage{
geo_choropleth(col, palette = NULL, convert2density = FALSE, n = 5,
  style = "pretty", breaks = NULL, labels = NULL,
  auto.palette.mapping = TRUE, contrast = 1, colorNA = "grey65",
  thres.poly = 1e-05, total.area.km2 = NA)
}
\arguments{
  \item{col}{name of the data variable that is contained in
  \code{shp}}

  \item{palette}{palette name. See
  \code{RColorBrewer::display.brewer.all()} for options.
  Use a \code{"-"} as prefix to reverse the palette. By
  default, \code{"RdYlGn"} is taken for numeric variables
  and \code{"Dark2"} for categorical variables.}

  \item{n}{preferred number of classes (in case \code{col}
  is a numeric variable)}

  \item{convert2density}{boolean that determines whether
  \code{col} is converted to a density variable. Should be
  \code{TRUE} when \code{col} consists of absolute numbers.
  Note that the conversion to densities is an approximation
  where the total area size is given by the argument
  \code{total.area.km2}.}

  \item{style}{method to cut the color scale (in case
  \code{col} is a numeric variable): "fixed", "equal",
  "pretty", "quantile", "kmeans"}

  \item{breaks}{in case \code{style=="fixed"}, breaks
  should be specified}

  \item{labels}{labels of the classes}

  \item{auto.palette.mapping}{When diverging colour
  palettes are used (i.e. "RdBu") this method automatically
  maps colors to values such that the middle colors (mostly
  white or yellow) are assigned to values of 0, and the two
  sides of the color palette are assigned to negative
  respectively positive values.}

  \item{contrast}{number between 0 and 1 (default) that
  determines the contrast of the palette. Only applicable
  when \code{auto.palette.mapping=TRUE}}

  \item{colorNA}{color used to missing values}

  \item{thres.poly}{number that specifies the threshold at
  which polygons are taken into account. The number itself
  corresponds to the proportion of the area sizes of the
  polygons to the total polygon size.}

  \item{total.area.km2}{total area size in km2. Needed if
  \code{convert2density=TRUE}.}
}
\value{
\code{\link{geo-object}}
}
\description{
This layer speficies a choropleth. A color palette is
mapped to a data variable. By default, a divering color
palette is used for numeric variables and a qualitative
palette for categorical variables.
}
\examples{
# World examples
data(World)
geo_shape(World) +
 	geo_choropleth("pop_est_dens", style="fixed", breaks=c(0, 5, 20, 100, 250, 1000, 20000), palette="YlOrRd", auto.palette.mapping=FALSE) +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", cex.lowerbound=.4, bg.alpha=0) +
 	geo_theme_World(legend.profile="text", title="Population density per km2", title.bg.color=TRUE)

geo_shape(World) +
 	geo_choropleth("income_grp", palette="-Oranges") +
 	geo_borders() +
 	geo_bubblemap("pop_est", col="blue") +
 #	geo_text("iso_a3", cex="AREA3") +
 	geo_theme_World("Income classification", legend.bubble.size.title="Population")

# Europe example
data(Europe)
geo_shape(Europe) +
 	geo_choropleth("gdp_cap_est", style="kmeans") +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", scale=2, bg.alpha=0) +
 	geo_theme_Europe("GDP per capita")

# Netherlands example
data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_muni) +
	geo_choropleth(col="pop", convert2density=TRUE, style="kmeans", total.area.km2=41543) +
	geo_borders() +
	geo_shape(NLD_prov) +
	geo_borders(, lwd=2) +
	geo_theme_NLD(title="Population (per km2)", legend.digits=0)
}
\seealso{
\code{\link{geo_fill}}
}

