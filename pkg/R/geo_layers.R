#' Specify the shape object
#' 
#' This layer specifies the shape object, which is one of \code{\link[sp:SpatialPolygons]{SpatialPolygons}}, \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}, \code{\link[sp:SpatialPoints]{SpatialPoints}}, and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}}.
#' 
#' @param shp shape object. For \code{\link{geo_fill}} and \code{\link{geo_bubbles}}, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} or a \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} is requied. \code{\link[sp:SpatialPoints]{SpatialPoints}} and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} are only used for \code{\link{geo_bubbles}}.
#' @param projection character that determines the projectino. Either a \code{PROJ.4} character string (see \url{http://trac.osgeo.org/proj/}), of one of the following shortcuts: 
#' \describe{
#'    	\item{\code{"longlat"}}{Not really a projection, but a plot of the longitude-latitude coordinates.} 
#'    	\item{\code{"wintri"}}{Winkel Tripel (1921). Popular projection that is useful in world maps. It is the standard of world maps made by the National Geographic Society. Type: compromise} 
#'    	\item{\code{"robin"}}{Robinson (1963). Another popular projection for world maps. Type: compromise}
#'    	\item{\code{"eck4"}}{Eckert IV (1906). Projection useful for world maps. Area sizes are preserved, which makes it particularly useful for truthful choropleths. Type: equal-area}
#'    	\item{\code{"hd"}}{Hobo-Dyer (2002). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"gall"}}{Gall (Peters) (1855). Another projection useful for world maps in which area sizes are preserved. Type: equal-area}
#'    	\item{\code{"merc"}}{Mercator (1569). Projection in which shapes are locally preserved. However, areas close to the poles are inflated. Google Maps uses a close variant of the Mercator. Type: conformal}
#'    	\item{\code{"mill"}}{Miller (1942). Projetion based on Mercator, in which poles are displayed. Type: compromise}
#'    	\item{\code{"eqc0"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The equator is the standard parallel. Also known as Plate Carr\'ee. Type: equidistant}
#'    	\item{\code{"eqc30"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 30 is the standard parallel. Type: equidistant}
#'    	\item{\code{"eqc45"}}{Equirectangular (120). Projection in which distances along meridians are conserved. The latitude of 45 is the standard parallel. Also known as Gall isographic. Type: equidistant}
#'    	\item{\code{"rd"}}{Rijksdriehoekstelsel. Triangulation coordinate system used in the Netherlands.}}
#'    	See \url{http://en.wikipedia.org/wiki/List_of_map_projections} for a overview of projections.
#'    	By default, the projection is used that is defined in the \code{shp} object itself.
#' @param xlim limits of the x-axis
#' @param ylim limits of the y-axis
#' @param relative boolean that determines whether relative values are used for \code{xlim} and \code{ylim} or absolute. Note: relative values will depend on the current bounding box (bbox) of the first shape object.
#' @param bbox bounding box, which is a 2x2 matrix that consists absolute \code{xlim} and \code{ylim} values. If specified, it overrides the \code{xlim} and \code{ylim} parameters.
#' @export
#' @example ../examples/geo_shape.R
#' @return \code{\link{geo-element}}
geo_shape <- function(shp, 
					  projection=NULL, 
					  xlim = NULL,
					  ylim = NULL,
					  relative = TRUE,
					  bbox = NULL) {
	shp_name <- deparse(substitute(shp))
	g <- list(geo_shape=as.list(environment()))
	class(g) <- "geo"
	g
}



#' Draw polygon borders
#' 
#' This layer defines the borders of the polygons. Color, line width and line type can be set.
#' 
#' @param col line color
#' @param lwd line width (see \code{\link[graphics:par]{par}})
#' @param lty line type (see \code{\link[graphics:par]{par}})
#' @export
#' @example ../examples/geo_borders.R
#' @return \code{\link{geo-element}}
geo_borders <- function(col="grey40", lwd=1, lty="solid") {
	g <- list(geo_borders=as.list(environment()))
	class(g) <- "geo"
	g
}


#' Add text labels
#' 
#' This layer adds text labels
#' 
#' @param text name of the variable in the shape object that contains the text labels
#' @param cex relative size of the text labels. Eiter one number, a name of a numeric variable in the shape data that is used to scale the sizes proportionally, or one of the following special cases
#' \describe{
#' 		\item{\code{"AREA"}:}{text size is proportional to the squared root of the area size of the polygons.}
#' 		\item{\code{"AREAx"}:}{text size is proportional to the x-th root of the area size of the polygons.}}
#' @param fontcolor relative size of the text labels
#' @param fontface font face of the text labels
#' @param fontfamily font family of the text labels
#' @param bg.color background color of the text labels
#' @param bg.alpha number between 0 and 255 that specifies the transparancy of the text background (0 is totally transparent, 255 is solid background)
#' @param cex.lowerbound lowerbound for \code{cex}. Needed to ignore the tiny labels in case \code{cex} is a variable.
#' @param print.tiny boolean that determines if tiny labels (which size is smaller than \code{cex.lowerbound}) are print at size \code{cex.lowerbound}
#' @param scale scalar needed in case cex is based 
#' @param xmod horizontal position modification of the text, relatively where 0 means no modification, and 1 means the total width of the frame. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the text labels. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the text to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @export
#' @example ../examples/geo_text.R
#' @return \code{\link{geo-element}}
geo_text <-  function(text, cex=1, fontcolor=NA, fontface="plain", fontfamily="sans", bg.color="#888888", bg.alpha=100, cex.lowerbound=.2, print.tiny=FALSE, scale=1, xmod=0, ymod=0) {
	g <- list(geo_text=list(text=text, text.cex=cex, text.fontcolor=fontcolor, text.fontface=fontface, text.fontfamily=fontfamily, text.bg.color=bg.color, text.bg.alpha=bg.alpha,
							text.cex.lowerbound=cex.lowerbound, text.print.tiny=print.tiny, text.scale=scale, text.xmod=xmod, text.ymod=ymod))
	class(g) <- "geo"
	g
}


#' Draw spatial lines
#' 
#' This layer draw spatial lines.
#' 
#' @param col color of the lines. Either a color value or a data variable name.
#' @param lwd line width
#' @param lty line type
#' @param palette color palette, used if \code{col} is a data variable
#' @param by logical. If \code{TRUE} and \code{col} is a data variable, draw small multiples, one for each level
#' @export
#' @return \code{\link{geo-element}}
geo_lines <- function(col="red", lwd=1, lty="solid", palette=NULL, by=FALSE) {
	g <- list(geo_lines=list(lines.col=col, lines.lwd=lwd, lines.lty=lty, lines.by=by))
	class(g) <- "geo"
	g
}


#' Draw choropleth
#' 
#' This layer speficies a choropleth. A color palette is mapped to a data variable. By default, a divering color palette is used for numeric variables and a qualitative palette for categorical variables.
#' 
#' @param col either a single color value or a name of the data variable that is contained in \code{shp}. In the latter case, a choropleth is drawn.
#' @param palette palette name. See \code{RColorBrewer::display.brewer.all()} for options. Use a \code{"-"} as prefix to reverse the palette. By default, \code{"RdYlGn"} is taken for numeric variables and \code{"Dark2"} for categorical variables.
#' @param n preferred number of classes (in case \code{col} is a numeric variable)
#' @param convert2density boolean that determines whether \code{col} is converted to a density variable. Should be \code{TRUE} when \code{col} consists of absolute numbers. Note that the conversion to densities is an approximation where the total area size is given by the argument \code{total.area.km2}.
#' @param style method to cut the color scale (in case \code{col} is a numeric variable): "fixed", "equal", "pretty", "quantile", "kmeans"
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast number between 0 and 1 (default) that determines the contrast of the palette. Only applicable when \code{auto.palette.mapping=TRUE}
#' @param colorNA color used to missing values
#' @param thres.poly number that specifies the threshold at which polygons are taken into account. The number itself corresponds to the proportion of the area sizes of the polygons to the total polygon size. 
#' @param total.area.km2 total area size in km2. Needed if \code{convert2density=TRUE}.
#' @export
#' @example ../examples/geo_fill.R
#' @return \code{\link{geo-element}}	
geo_fill <- function(col="grey90", 
						    palette = NULL,
						    convert2density = FALSE,
						    n = 5,
						    style = "pretty",
							breaks = NULL,
						    labels = NULL,
							auto.palette.mapping = TRUE,
							contrast = 1,
							colorNA = "grey65",
							thres.poly = 1e-05,
							total.area.km2=NA) {
	
	g <- list(geo_fill=as.list(environment()))
	class(g) <- "geo"
	g
}	

#' Draw bubblemap
#' 
#' This layer speficies a bubblemap. Both colors and sizes of the bubbles can be mapped to data variables. 
#' 
#' @param size \code{shp} data variable that determines the bubble sizes. Multiple variable names create small multiples
#' @param col color(s) of the bubble. Either a color (vector), or categorical variable name(s). Multiple variable names create small multiples
#' @param border color of the bubble borders. If \code{NA} (default), no bubble borders are drawn.
#' @param scale bubble size multiplier number. 
#' @param n preferred number of color scale classes. Only applicable when \code{col} is a numeric variable name.
#' @param style method to cut the color scale: "fixed", "equal", "pretty", "quantile", "kmeans". Only applicable when \code{col} is a numeric variable name.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param palette color palette (see \code{RColorBrewer::display.brewer.all}) for the bubbles. Only when \code{col} is set to a variable.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast number between 0 and 1 (default) that determines the contrast of the palette. Only applicable when \code{auto.palette.mapping=TRUE} and \code{col} is a numeric variable name. 
#' @param colorNA colour for missing values
#' @param xmod horizontal position modification of the bubbles, relatively where 0 means no modification, and 1 means the total width of the frame. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the bubbles. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the bubbles to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @param by logical. If \code{TRUE} and \code{col} is a data variable, then small multiples are generated, one for each level (NOT WORKING YET) 
#' @export
#' @example ../examples/geo_bubbles.R
#' @return \code{\link{geo-element}}
geo_bubbles <- function(size=1, col="blueviolet",
						  border=NA,
						  scale=1,
						  n = 5, style = "pretty",
						  breaks = NULL,
						  palette = NULL,
						  labels = NULL,
						  auto.palette.mapping = TRUE,
						  contrast = 1,
						  colorNA = "#FF1414",
						  xmod = 0,
						  ymod = 0,
						  by=FALSE) {
	g <- list(geo_bubbles=list(bubble.size=size, bubble.col=col, bubble.border=border,
								 bubble.scale=scale,
								 n=n, style=style, breaks=breaks, palette=palette, labels=labels,
								 auto.palette.mapping=auto.palette.mapping, contrast=contrast,
								 colorNA=colorNA,
								 bubble.xmod=xmod,
								 bubble.ymod=ymod,
								 bubble.by=by))
	class(g) <- "geo"
	g
}


#' Small multiples grid
#' 
#' This layer specifies how the different plots are placed in a grid. The number of rows and columns can be specified here, as well as whether the scales are free (i.e. independent of each other).
#' 
#' @param ncol number of columns of the small multiples grid
#' @param nrow number of rows of the small multiples grid
#' @param free.scales logical. Should all scales of the plotted data variables be free, i.e. independent of each other? Possible data variables are color from \code{\link{geo_fill}}, color and size from \code{\link{geo_bubbles}} and line color from \code{\link{geo_lines}}.
#' @param free.scales.fill logical. Should the color scale for the choropleth be free?
#' @param free.scales.bubble.size logical. Should the bubble size scale for the bubblemap be free?
#' @param free.scales.bubble.col logical. Should the color scale for the bubblemap be free?
#' @param free.scales.line.col Should the line color scale be free?
#' @export
#' @example ../examples/geo_grid.R
#' @return \code{\link{geo-element}}
geo_grid <- function(ncol=NULL, nrow=NULL, 
					 free.scales=TRUE,
					 free.scales.fill=free.scales,
					 free.scales.bubble.size=free.scales,
					 free.scales.bubble.col=free.scales,
					 free.scales.line.col=free.scales
					 )	{
	g <- list(geo_grid=as.list(environment()))
	class(g) <- "geo"
	attr(g, "call") <- names(match.call(expand.dots = TRUE)[-1])
	g
}



#' Stacking of geo elements
#' 
#' The plus operator allows you to stack \code{\link{geo-element}s}. Always start with \code{\link{geo_shape}} to specify the shape object. If multile layers are used, each layer should start with a \code{\link{geo_shape}} element.
#' 
#' @param e1 first \code{\link{geo-element}}
#' @param e2 second \code{\link{geo-element}}
#' @export
"+.geo" <- function(e1, e2) {
	g <- c(e1,e2)
	class(g) <- "geo"
	g
}



