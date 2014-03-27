\name{geo_bubblemap}
\alias{geo_bubblemap}
\title{Draw bubblemap}
\usage{
geo_bubblemap(size = NULL, col = NULL, border = NA, scale = 1, n = 5,
  style = "pretty", breaks = NULL, palette = NULL, labels = NULL,
  auto.palette.mapping = TRUE, contrast = 1, colorNA = "#FF1414")
}
\arguments{
  \item{size}{\code{shp} data variable that determines the
  bubble sizes. Multiple variable names create small
  multiples}

  \item{col}{color(s) of the bubble. Either a color
  (vector), or categorical variable name(s). Multiple
  variable names create small multiples}

  \item{borders}{color of the bubble borders. If \code{NA}
  (default), no bubble borders are drawn.}

  \item{scale}{bubble size multiplier number.}

  \item{n}{preferred number of color scale classes. Only
  applicable when \code{col} is a numeric variable name.}

  \item{style}{method to cut the color scale: "fixed",
  "equal", "pretty", "quantile", "kmeans". Only applicable
  when \code{col} is a numeric variable name.}

  \item{breaks}{in case \code{style=="fixed"}, breaks
  should be specified}

  \item{palette}{color palette (see
  \code{RColorBrewer::display.brewer.all}) for the bubbles.
  Only when \code{col} is set to a variable.}

  \item{labels}{labels of the classes}

  \item{auto.palette.mapping}{When diverging colour
  palettes are used (i.e. "RdBu") this method automatically
  maps colors to values such that the middle colors (mostly
  white or yellow) are assigned to values of 0, and the two
  sides of the color palette are assigned to negative
  respectively positive values.}

  \item{contrast}{number between 0 and 1 (default) that
  determines the contrast of the palette. Only applicable
  when \code{auto.palette.mapping=TRUE} and \code{col} is a
  numeric variable name.}

  \item{colorNA}{colour for missing values}
}
\value{
\code{\link{geo-object}}
}
\description{
This layer speficies a bubblemap. Both colors and sizes of
the bubbles can be mapped to data variables.
}
\seealso{
\code{\link{geo_bubblemap}}
}

