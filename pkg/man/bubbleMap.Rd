\name{bubbleMap}
\alias{bubbleMap}
\title{Create bubble map}
\usage{
  bubbleMap(shp, x, col = "red", free.scales = FALSE,
    scale = 1, n = 5, style = "pretty", breaks = NULL,
    palette = NULL, auto.palette.mapping = TRUE,
    contrast = 1, x.shp.col = NULL,
    palette.shp.col = "Set3", title = NULL,
    title.cex = 1.5, show.legend.sizes = TRUE,
    show.legend.colors = NULL, legend.labels = NULL,
    legend.plot.size = c(0.15, 0.35), legend.cex = 0.8,
    legend.digits = 3, plot.bubble.borders = TRUE,
    plot.bg = "white", plot.borders = "black",
    colorNA = "#FF1414", mfcol = length(x), mfrow = 1,
    vp = NULL, ...)
}
\arguments{
  \item{shp}{a shape object}

  \item{x}{\code{shp} data variable that determines the
  bubble sizes. Multiple variable names create small
  multiples}

  \item{col}{colour(s) of the bubble. Either a single
  colour, or a categorical data variable name}

  \item{free.scales}{in case of small multiples, should
  each have its own colour scale?}

  \item{scale}{number that is used to scale the bubbles}

  \item{n}{preferred number of color scale classes. Only
  applicable when \code{col} is a numeric variable name.}

  \item{style}{method to cut the color scale: "fixed",
  "equal", "pretty", "quantile", "kmeans". Only applicable
  when \code{col} is a numeric variable name.}

  \item{breaks}{in case \code{style=="fixed"}, breaks
  should be specified}

  \item{palette}{colour palette (see
  \code{RColorBrewer::display.brewer.all}) for the bubble.
  Only when \code{col} is set to a variable.}

  \item{auto.palette.mapping}{When diverging colour
  palettes are used (i.e. "RdBu") this method automatically
  maps values to colours such that negative values get a
  left-sided colour of the palette (for instance red),
  zeros get the middle colour, and positive values a
  right-sided colour (for instance blue). Only applicable
  when \code{col} is a numeric variable name.}

  \item{contrast}{number between 0 and 1 (default) that
  determines the contrast of the palette. Only applicable
  when \code{auto.palette.mapping=TRUE} and \code{col} is a
  numeric variable name.}

  \item{x.shp.col}{name of a categorical variable (factor)
  that determines the fill of the shapes}

  \item{palette.shp.col}{palette to fill the shapes (only
  applicable is \code{x.shp.col} is specified}

  \item{title}{title of the legend}

  \item{title.cex}{font size of the title}

  \item{show.legend.sizes}{show legend for bubble sizes?}

  \item{show.legend.colors}{show legend for bubble colors
  (if col is a variable)?}

  \item{legend.labels}{custom legend labels}

  \item{legend.plot.size}{Vector of two values,
  respectively width and height, that specify the size of
  the legend plot. The values are ratios of the total plot
  width and height.}

  \item{legend.cex}{legend labels size}

  \item{legend.digits}{number of printed digits of the
  break values. Only applicable when \code{col} is a
  numeric variable name.}

  \item{plot.bubble.borders}{plot the borders of the
  bubbles?}

  \item{plot.bg}{background colour}

  \item{plot.borders}{colour of the shape borders}

  \item{colorNA}{colour for missing values}

  \item{mfcol}{the number of columns when small multiples
  are created (i.e. when \code{x} contains multiple
  variable names). See also \code{mfrow}. If \code{x}
  contains more variable names than \code{mfcol*mfrow},
  then multiple plots are created, which could be useful
  for animations. By default all choropleths are drawn next
  to each other.}

  \item{mfrow}{the number of rows when small multiples are
  created. See \code{mfcol}}

  \item{vp}{\code{\link[grid:viewport]{viewport}} to draw
  in. By default it is not specified, which means that a
  new plot is created. Useful when drawing customized small
  multiples.}

  \item{...}{other arguments are passed on to
  \code{cartoMap}}
}
\description{
  This function creates a bubble map.
}
\examples{
## load shape files
options(shp_dir=system.file("shapes", package="geoNL"))

## load shape
shp.gm <- getShape("gm", 2012)

## load data
data(NLpop)

## add derived variables
NLpop <- transform(NLpop, perc.male=total.male/total*100, perc.female=total.female/total*100)

## append data to shape file
shp.gm <- appendData(shp.gm, data=NLpop, key.data="gm.name",key.shp="GM_NAAM")

## draw absolute population totals
bubbleMap(shp.gm, x="total")

## map gender ratio to color
shp.gm$gender <- ((shp.gm$perc.male/100)-.5) / sqrt(shp.gm$total) * 10000
shp.pv <- getShape("pv", 2012)

bubbleMap(shp.gm, x="total", col="gender", palette="RdBu", n=5, style="kmeans", legend.labels=c("more women", "slightly more women", "equal", "slightly more men", "more men"),
		  show.legend.sizes=TRUE, shp2=shp.pv, lwd2=2)


}
\keyword{bubbleMap}

