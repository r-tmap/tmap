#' Specify the shape object
#' 
#' Creates a \code{\link{tmap-element}} that specifies the shape object. Also the used projection and covered area (bounding box) can be set.
#' 
#' @param shp shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#' }
#'For drawing layers \code{\link{tm_fill}} and \code{\link{tm_borders}}, 1 is required. For drawing layer \code{\link{tm_lines}} 3 is required. Layers \code{\link{tm_bubbles}} and \code{\link{tm_text}}, accept any of them. 
#' @param projection character that determines the projection. Either a \code{PROJ.4} character string (see \url{http://trac.ostm.org/proj/}), of one of the following shortcuts: 
#' \describe{
#'    	\item{\code{"longlat"}}{Not really a projection, but a plot of the longitude-latitude coordinates.} 
#'    	\item{\code{"wintri"}}{Winkel Tripel (1921). Popular projection that is useful in world maps. It is the standard of world maps made by the National tmgraphic Society. Type: compromise} 
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
#' @seealso \code{\link{read_shape}} to read ESRI shape files, \code{\link{set_projection}}, \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}} 
#' @example ../examples/tm_shape.R
#' @return \code{\link{tmap-element}}
tm_shape <- function(shp, 
					  projection=NULL, 
					  xlim = NULL,
					  ylim = NULL,
					  relative = TRUE,
					  bbox = NULL) {
	shp_name <- deparse(substitute(shp))
	g <- list(tm_shape=as.list(environment()))
	class(g) <- "tmap"
	g
}



#' Draw polygon borders
#' 
#' Creates a \code{\link{tmap-element}} that defines the borders of the polygons. Line color, width, and type can be set.
#' 
#' @param col line color
#' @param lwd line width (see \code{\link[graphics:par]{par}})
#' @param lty line type (see \code{\link[graphics:par]{par}})
#' @export
#' @example ../examples/tm_borders.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_borders <- function(col="grey40", lwd=1, lty="solid") {
	g <- list(tm_borders=as.list(environment()))
	class(g) <- "tmap"
	g
}


#' Add text labels
#' 
#' Creates a \code{\link{tmap-element}} that adds text labels
#' 
#' @param text name of the variable in the shape object that contains the text labels
#' @param cex relative size of the text labels. Eiter one number, a name of a numeric variable in the shape data that is used to scale the sizes proportionally, or \code{AREA} where the text size is proportional to the the area size of the polygons.
#' @param root root number to which the font sizes are scaled. Only applicable if \code{cex} is a variable name or \code{"AREA"}. If \code{root=2}, the square root is taken, if \code{root=3} the cube root etc.
#' @param fontcolor relative size of the text labels
#' @param fontface font face of the text labels
#' @param fontfamily font family of the text labels
#' @param case case of the font. Use "upper" to generate upper-case text, "lower" to generate lower-case text, and use \code{NA} to leave the text as is.
#' @param bg.color background color of the text labels. By default, \code{bg.color=NA}, so no background is drawn.
#' @param bg.alpha number between 0 and 255 that specifies the transparancy of the text background (0 is totally transparent, 255 is solid background). The default value is 100.
#' @param cex.lowerbound lowerbound for \code{cex}. Needed to ignore the tiny labels in case \code{cex} is a variable.
#' @param print.tiny boolean that determines if tiny labels (which size is smaller than \code{cex.lowerbound}) are print at size \code{cex.lowerbound}
#' @param scale scalar needed in case cex is based 
#' @param xmod horizontal position modification of the text, relatively where 0 means no modification, and 1 means the total width of the frame. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the text labels. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the text to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @export
#' @example ../examples/tm_text.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_text <-  function(text, cex=1, root=3, fontcolor=NA, fontface="plain", fontfamily="sans", case=NA, bg.color=NA, bg.alpha=100, cex.lowerbound=.4, print.tiny=FALSE, scale=1, xmod=0, ymod=0) {
	g <- list(tm_text=list(text=text, text.cex=cex, root=root, text.fontcolor=fontcolor, text.fontface=fontface, text.fontfamily=fontfamily, text.case=case, text.bg.color=bg.color, text.bg.alpha=bg.alpha,
							text.cex.lowerbound=cex.lowerbound, text.print.tiny=print.tiny, text.scale=scale, text.xmod=xmod, text.ymod=ymod))
	class(g) <- "tmap"
	g
}


#' Draw spatial lines
#' 
#' Creates a \code{\link{tmap-element}} that draw spatial lines.
#' 
#' @param col color of the lines. Either a color value or a data variable name.
#' @param lwd line width
#' @param lty line type
#' @param scale line width multiplier number. 
#' @param n preferred number of color scale classes. Only applicable when \code{lwd} is the name of a numeric variable.
#' @param style method to cut the color scale: "fixed", "equal", "pretty", "quantile", "kmeans". Only applicable when \code{lwd} is the name of a numeric variable.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param palette color palette (see \code{RColorBrewer::display.brewer.all}) for the lines. Only when \code{col} is set to a variable.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values. In this case of line widths, obviously only the positive side is used. 
#' @param contrast number between 0 and 1 (default) that determines the contrast of the palette. Only applicable when \code{auto.palette.mapping=TRUE}
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA color used for missing values
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @export
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @example ../examples/tm_lines.R
#' @return \code{\link{tmap-element}}
tm_lines <- function(col="red", lwd=1, lty="solid", 
					  scale=1,
					  n = 5, style = "pretty",
					  breaks = NULL,
					  palette = NULL,
					  labels = NULL,
					  auto.palette.mapping = TRUE,
					  contrast = 1,
					  max.categories = 12, 
					  colorNA = "grey65",
					  textNA = "Missing") {
	g <- list(tm_lines=list(lines.col=col, lines.lwd=lwd, lines.lty=lty, lines.scale=scale,
							 n=n, style=style, breaks=breaks, palette=palette, labels=labels,
							 auto.palette.mapping=auto.palette.mapping,
							 max.categories=max.categories,
							 contrast=contrast, colorNA=colorNA, textNA=textNA))
	class(g) <- "tmap"
	g
}


#' Fill polygons
#' 
#' Creates a \code{\link{tmap-element}} that fills polygons. Either a fixed color is used, or a color palette is mapped to a data variable. By default, a divering color palette is used for numeric variables and a qualitative palette for categorical variables.
#' 
#' @param col either a single color value or a name of the data variable that is contained in \code{shp}. In the latter case, a choropleth is drawn.
#' @param palette palette name. See \code{RColorBrewer::display.brewer.all()} for options. Use a \code{"-"} as prefix to reverse the palette. By default, \code{"RdYlGn"} is taken for numeric variables and \code{"Dark2"} for categorical variables.
#' @param n preferred number of classes (in case \code{col} is a numeric variable)
#' @param convert2density boolean that determines whether \code{col} is converted to a density variable. Should be \code{TRUE} when \code{col} consists of absolute numbers. The area size is either approximated from the shape object, or given by the argument \code{area}.
#' @param area Name of the data variable that contains the area sizes in squared kilometer.
#' @param style method to cut the color scale (in case \code{col} is a numeric variable): "fixed", "equal", "pretty", "quantile", "kmeans"
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast number between 0 and 1 (default) that determines the contrast of the palette. Only applicable when \code{auto.palette.mapping=TRUE}
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA color used for missing values
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @param thres.poly number that specifies the threshold at which polygons are taken into account. The number itself corresponds to the proportion of the area sizes of the polygons to the total polygon size. 
#' @export
#' @example ../examples/tm_fill.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}	
tm_fill <- function(col="grey90", 
						    palette = NULL,
						    convert2density = FALSE,
					 		area = NULL,
						    n = 5,
						    style = "pretty",
							breaks = NULL,
						    labels = NULL,
							auto.palette.mapping = TRUE,
							contrast = 1,
					 		max.categories = 12,
					 		colorNA = "grey65",
					 		textNA = "Missing",
							thres.poly = 1e-05) {
	
	g <- list(tm_fill=as.list(environment()))
	class(g) <- "tmap"
	g
}	

#' Draw bubbles
#' 
#' Creates a \code{\link{tmap-element}} that draws bubbles. Both colors and sizes of the bubbles can be mapped to data variables. 
#' 
#' @param size \code{shp} data variable that determines the bubble sizes. Multiple variable names create small multiples
#' @param col color(s) of the bubble. Either a color (vector), or categorical variable name(s). Multiple variable names create small multiples
#' @param border.lwd line width of the bubble borders. If \code{NA} (default), no bubble borders are drawn.
#' @param border.col color of the bubble borders.
#' @param scale bubble size multiplier number. 
#' @param size.lim vector of two limit values of the \code{size} variable. Only bubbles are drawn whose value is greater than or equal to the first value. Bubbles whose values exceed the second value are drawn at the size of the second value. Only applicable when \code{size} is the name of a numeric variable of \code{shp}
#' @param n preferred number of color scale classes. Only applicable when \code{col} is a numeric variable name.
#' @param style method to cut the color scale: "fixed", "equal", "pretty", "quantile", "kmeans". Only applicable when \code{col} is a numeric variable name.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param palette color palette (see \code{RColorBrewer::display.brewer.all}) for the bubbles. Only when \code{col} is set to a variable.
#' @param labels labels of the classes
#' @param auto.palette.mapping When diverging colour palettes are used (i.e. "RdBu") this method automatically maps colors to values such that the middle colors (mostly white or yellow) are assigned to values of 0, and the two sides of the color palette are assigned to negative respectively positive values.
#' @param contrast number between 0 and 1 (default) that determines the contrast of the palette. Only applicable when \code{auto.palette.mapping=TRUE} and \code{col} is a numeric variable name. 
#' @param max.categories in case \code{col} is the name of a categorical variable, this value determines how many categories (levels) it can have maximally. If the number of levels is higher than \code{max.categories}, then levels are combined.
#' @param colorNA colour for missing values
#' @param textNA text used for missing values. Use \code{NA} to omit text for missing values in the legend
#' @param xmod horizontal position modification of the bubbles, relatively where 0 means no modification, and 1 means the total width of the frame. Either a single number for all polygons, or a numeric variable in the shape data specifying a number for each polygon. Together with \code{ymod}, it determines position modification of the bubbles. In most coordinate systems (projections), the origin is located at the bottom left, so negative \code{xmod} move the bubbles to the left, and negative \code{ymod} values to the bottom.
#' @param ymod vertical position modification. See xmod.
#' @export
#' @example ../examples/tm_bubbles.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_bubbles <- function(size=1, col="blueviolet",
						  border.lwd=NA,
						  border.col="black",
						  scale=1,
						  size.lim=NA,
						  n = 5, style = "pretty",
						  breaks = NULL,
						  palette = NULL,
						  labels = NULL,
						  auto.palette.mapping = TRUE,
						  contrast = 1,
						  max.categories = 12,
						  colorNA = "#FF1414",
						  textNA = "Missing",
						  xmod = 0,
						  ymod = 0) {
	g <- list(tm_bubbles=list(bubble.size=size, bubble.col=col, bubble.border.lwd=border.lwd,
							   bubble.border.col=border.col,
								 bubble.scale=scale,
								 size.lim=size.lim,
								 n=n, style=style, breaks=breaks, palette=palette, labels=labels,
								 auto.palette.mapping=auto.palette.mapping,
								 max.categories=max.categories,
								 contrast=contrast,
								 colorNA=colorNA,
								 textNA=textNA,
								 bubble.xmod=xmod,
								 bubble.ymod=ymod))
	class(g) <- "tmap"
	g
}


#' Small multiples
#' 
#' Creates a \code{\link{tmap-element}} that specifies how small multiples are placed in a facet grid. Either the argument \code{by} should be specified, i.e. the name of a variable by which the data is grouped, or multiple variable names sould be provided with \code{\link{tm_fill}}, \code{\link{tm_lines}}, or \code{\link{tm_bubbles}}. In this function, the number of rows and columns can be specified, as well as whether the scales are free (i.e. independent of each other).
#' 
#' @param by data variable name by which the data is split
#' @param ncol number of columns of the small multiples grid
#' @param nrow number of rows of the small multiples grid
#' @param free.scales logical. Should all scales of the plotted data variables be free, i.e. independent of each other? Possible data variables are color from \code{\link{tm_fill}}, color and size from \code{\link{tm_bubbles}} and line color from \code{\link{tm_lines}}.
#' @param free.scales.fill logical. Should the color scale for the choropleth be free?
#' @param free.scales.bubble.size logical. Should the bubble size scale for the bubble map be free?
#' @param free.scales.bubble.col logical. Should the color scale for the bubble map be free?
#' @param free.scales.line.col Should the line color scale be free?
#' @param free.scales.line.lwd Should the line width scale be free?
#' @export
#' @example ../examples/tm_facets.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_facets <- function(by=NULL, ncol=NULL, nrow=NULL, 
					   free.scales=is.null(by),
					   free.scales.fill=free.scales,
					   free.scales.bubble.size=free.scales,
					   free.scales.bubble.col=free.scales,
					   free.scales.line.col=free.scales,
					   free.scales.line.lwd=free.scales
					   ) {
	g <- list(tm_facets=as.list(environment()))
	class(g) <- "tmap"
	attr(g, "call") <- names(match.call(expand.dots = TRUE)[-1])
	g
}

#' Coordinate grid lines
#' 
#' Creates a \code{\link{tmap-element}} that draws coordinate grid lines.
#' 
#' @param n.x Prefered number of grid lines for the x axis.
#' @param n.y Prefered number of grid lines for the y axis.
#' @param col Color for the grid lines.
#' @param labels.cex font size of the tick labels
#' @param labels.col font color fo the tick labels
#' @param on.top Boolean that determines whether the grid lines are drawn op top of the map (\code{TRUE}) or under the map (\code{FALSE}).
#' @export
tm_grid <- function(n.x=8,
					 n.y=8,
					 col="grey50",
					 labels.cex=.75,
					 labels.col="grey20",
					 on.top=TRUE) {
	g <- list(tm_grid=as.list(environment()))
	names(g$tm_grid) <- paste("grid", names(g$tm_grid), sep=".")
	class(g) <- "tmap"
	attr(g, "call") <- names(match.call(expand.dots = TRUE)[-1])
	g
}

#' Stacking of tmap elements
#' 
#' The plus operator allows you to stack \code{\link{tmap-element}s}.
#' 
#' @param e1 first \code{\link{tmap-element}}
#' @param e2 second \code{\link{tmap-element}}
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @export
"+.tmap" <- function(e1, e2) {
	g <- c(e1,e2)
	class(g) <- "tmap"
	g
}



