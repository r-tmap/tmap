\name{choropleth}
\alias{choropleth}
\title{Create choropleeth}
\usage{
  choropleth(shp, x, n = 5, style = "pretty",
    free.scales = FALSE, breaks = NULL, palette = "RdBu",
    auto.palette.mapping = TRUE, contrast = 1, title = x,
    title.cex = 1.5, show.legend.text = TRUE,
    type.legend.plot = "hist", legend.labels = NULL,
    legend.plot.size = c(0.2, 0.35), legend.cex = 0.8,
    legend.digits = 2, plot.bg = "grey",
    plot.borders = "grey", colorNA = "#FF1414",
    mfcol = length(x), mfrow = 1, vp = NULL, ...)
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

  \item{free.scales}{in case of small multiples, should
  each have its own colour scale?}

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

  \item{title}{title of the choropleeth}

  \item{title.cex}{font size of the title}

  \item{show.legend.text}{Should the category names be
  printed? (TRUE be default)}

  \item{type.legend.plot}{One of the following: "hist" for
  the histogram of values of x (default), "bar" for a bar
  chart of sorted values, or "none" for hiding the plot}

  \item{legend.labels}{assign legend.labels. If not
  specified, legend labels will be automatically determined
  based on the break values (see also
  \code{legend.digits})}

  \item{legend.plot.size}{Vector of two values,
  respectively width and height, that specify the size of
  the legend plot. The values are ratios of the total plot
  width and height.}

  \item{legend.cex}{font size of the legend labels}

  \item{legend.digits}{number of printed digits of the
  break values}

  \item{plot.bg}{background colour}

  \item{plot.borders}{border colour}

  \item{colorNA}{colour used for missing values}

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

  \item{...}{other arguments}
}
\description{
  This function creates a choropleth.
}
\examples{
## load shape files
options(shp_dir=system.file("shapes", package="geoNL"))
		
shp.gm <- getShape("gm", 2012)


## load CBS-Statline data: NL population at 2012-01-01 at manucipality level
data(NLpop)

## add derived variables
NLpop <- transform(NLpop, perc.male=total.male/total*100, perc.female=total.female/total*100)

## append data to shape file
shp.gm <- appendData(shp.gm, data=NLpop, key.data="gm.name",key.shp="GM_NAAM")

## add population densities
shp.gm$pop.dens <- densities(shp.gm, "total")

## plot population density map on gm level
choropleth(shp.gm, "pop.dens", style="kmeans")

## draw province borders
shp.pv <- getShape("pv", 2012)
choropleth(shp.gm, "pop.dens", style="kmeans", shp2=shp.pv, lwd2=1)
}
\keyword{choropleth}

