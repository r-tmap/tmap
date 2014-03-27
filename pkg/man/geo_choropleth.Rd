\name{geo_choropleth}
\alias{geo_choropleth}
\title{Draw choropleth}
\usage{
geo_choropleth(col, palette = NULL, convert2density = FALSE, n = 5,
  style = "pretty", breaks = NULL, labels = NULL,
  auto.palette.mapping = TRUE, contrast = 1, colorNA = "#DDDDDD",
  total.area.km2 = NA)
}
\arguments{
  \item{col}{name of variable that is contained in
  \code{shp}}

  \item{palette}{palette name. See
  \code{RColorBrewer::display.brewer.all()} for options.
  Use a \code{"-"} as prefix to reverse the palette. By
  default, \code{"RdYlBu"} is taken for numeric variables,
  and \code{"Dark2"} for categorical variables.}

  \item{n}{preferred number of classes (in case \code{col}
  is a numeric variable)}

  \item{convert2density}{boolean that determines whether
  \code{col} is converted to a density variable. Should be
  \code{TRUE} when \code{col} consists of absolute
  numbers.}

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
\seealso{
\code{\link{geo_fill}}
}

