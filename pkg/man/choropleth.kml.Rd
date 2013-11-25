\name{choropleth.kml}
\alias{choropleth.kml}
\title{Create choropleeth}
\usage{
  choropleth.kml(shp, x, n = 5, style = "pretty",
    breaks = NULL, palette = "RdBu",
    auto.palette.mapping = TRUE, contrast = 1,
    legend.labels = NULL, legend.digits = 2,
    colorNA = "#FF1414", file = "./temp.kml", time = NULL,
    ...)
}
\arguments{
  \item{shp}{a shape object}

  \item{x}{names of variables that are included in the
  \code{shp} data (see \code{names(shp)})}

  \item{n}{preferred number of color scale classes}

  \item{style}{method to cut the color scale: "fixed",
  "equal", "pretty", "quantile", "kmeans"}

  \item{breaks}{in case \code{style=="fixed"}, breaks
  should be specified}

  \item{palette}{name of the used RColorBrewer palette (see
  \code{RColorBrewer::display.brewer.all})}

  \item{auto.palette.mapping}{When diverging colour
  palettes are used (i.e. "RdBu") this method automatically
  maps values to colours such that negative values get a
  left-sided colour of the palette (for instance red),
  zeros get the middle colour, and positive values a
  right-sided colour (for instance blue).}

  \item{contrast}{number between 0 and 1 (default) that
  determines the contrast of the palette. Only applicable
  when \code{auto.palette.mapping=TRUE}}

  \item{legend.labels}{assign legend.labels. If not
  specified, legend labels will be automatically determined
  based on the break values (see also
  \code{legend.digits})}

  \item{legend.digits}{number of printed digits of the
  break values}

  \item{colorNA}{colour used for missing values}

  \item{file}{filename of the kml file}

  \item{...}{other arguments}
}
\description{
  This function creates a choropleth.
}
\keyword{choropleth}

