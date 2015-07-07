#' Small multiples
#' 
#' Creates a \code{\link{tmap-element}} that specifies how small multiples are placed in a facet grid. Either the argument \code{by} should be specified, i.e. the name of a variable by which the data is grouped, or multiple variable names sould be provided with \code{\link{tm_fill}}, \code{\link{tm_lines}}, or \code{\link{tm_bubbles}}. In this function, the number of rows and columns can be specified, as well as whether the scales are free (i.e. independent of each other).
#' 
#' @param by data variable name by which the data is split
#' @param ncol number of columns of the small multiples grid
#' @param nrow number of rows of the small multiples grid
#' @param free.coords logical. If the \code{by} argument is specified, should each map has its own coordinate ranges?
#' @param drop.shapes logical. If the \code{by} argument is specified, should all non-selected shapes be dropped?
#' @param free.scales logical. Should all scales of the plotted data variables be free, i.e. independent of each other? Possible data variables are color from \code{\link{tm_fill}}, color and size from \code{\link{tm_bubbles}} and line color from \code{\link{tm_lines}}.
#' @param free.scales.fill logical. Should the color scale for the choropleth be free?
#' @param free.scales.bubble.size logical. Should the bubble size scale for the bubble map be free?
#' @param free.scales.bubble.col logical. Should the color scale for the bubble map be free?
#' @param free.scales.line.col Should the line color scale be free?
#' @param free.scales.line.lwd Should the line width scale be free?
#' @param free.scales.raster Should the color scale for raster layers be free?
#' @param inside.original.bbox If \code{free.coords}, should the bounding box of each small multiple be inside the original bounding box?
#' @param scale.factor Number that determines how the elements (e.g. font sizes, bubble sizes, line widths) of the small multiples are scaled in relation to the scaling factor of the shapes. The elements are scaled to the \code{scale.factor}th root of the scaling factor of the shapes. So, for \code{scale.factor=1}, they are scaled proportional to the scaling of the shapes. Since elements, especially text, are often too small to read, a higher value is recommended. By default, \code{scale.factor=2}.
#' @export
#' @example ../examples/tm_facets.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @return \code{\link{tmap-element}}
tm_facets <- function(by=NULL, ncol=NULL, nrow=NULL, 
					  free.coords=FALSE,
					  drop.shapes=FALSE,
					  free.scales=is.null(by),
					  free.scales.fill=free.scales,
					  free.scales.bubble.size=free.scales,
					  free.scales.bubble.col=free.scales,
					  free.scales.line.col=free.scales,
					  free.scales.line.lwd=free.scales,
					  free.scales.raster=free.scales,
					  inside.original.bbox=FALSE,
					  scale.factor=2) {
	calls <- names(match.call(expand.dots = TRUE)[-1])
	if ("free.scales" %in% calls) calls <- union(calls, c("free.scales.fill", "free.scales.bubble.size", "free.scales.bubble.col", "free.scales.line.col", "free.scales.line.lwd"))
	g <- list(tm_facets=c(as.list(environment()), list(call=calls)))
	class(g) <- "tmap"
	#attr(g, "call") <- names(match.call(expand.dots = TRUE)[-1])
	#g$call <- names(match.call(expand.dots = TRUE)[-1])
	g
}

#' Coordinate grid lines
#' 
#' Creates a \code{\link{tmap-element}} that draws coordinate grid lines.
#' 
#' @param n.x Prefered number of grid lines for the x axis.
#' @param n.y Prefered number of grid lines for the y axis.
#' @param col Color for the grid lines.
#' @param labels.size font size of the tick labels
#' @param labels.col font color fo the tick labels
#' @param labels.inside.frame Show labels inside the frame?
#' @param on.top Boolean that determines whether the grid lines are drawn op top of the map (\code{TRUE}) or under the map (\code{FALSE}).
#' @export
tm_grid <- function(n.x=8,
					n.y=8,
					col="grey50",
					labels.size=.75,
					labels.col="grey20",
					labels.inside.frame=FALSE,
					on.top=TRUE) {
	g <- list(tm_grid=as.list(environment()))
	names(g$tm_grid) <- paste("grid", names(g$tm_grid), sep=".")
	class(g) <- "tmap"
	attr(g, "call") <- names(match.call(expand.dots = TRUE)[-1])
	g
}

#' Credits text
#' 
#' Creates a text annotation that could be used for credits or acknowledgements.
#' 
#' @param text text
#' @param size relative text size
#' @param position position of the text. Vector of two values, specifing the x and y coordinates. Either this vector contains "left", "center" or "right" for the first value and "top", "center", or "bottom" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y value of the center of the text. By default, it is placed in the right bottom corner.
#' @param bg.color background color for the text
#' @param bg.alpha Transparency number between 0 (totally transparent) and 1 (not transparent). By default, the alpha value of the \code{bg.color} is used (normally 1).
#' @export
#' @example ../examples/tm_credits.R
tm_credits <- function(text,
					   size=.7,
					   position=c("right", "bottom"),
					   bg.color=NA,
					   bg.alpha=NA) {
	g <- list(tm_credits=as.list(environment()))
	names(g$tm_credits) <- paste("credits", names(g$tm_credits), sep=".")
	class(g) <- "tmap"
	attr(g, "call") <- names(match.call(expand.dots = TRUE)[-1])
	g
}


#' Scale bar
#' 
#' Creates a scale bar. By default, the coordinate units are assumed to be meters, and the map units in kilometers. This can be changed in \code{\link{tm_shape}}.
#' 
#' @param breaks breaks of the scale bar
#' @param size relative text size
#' @param position position of the text. Vector of two values, specifing the x and y coordinates. Either this vector contains "left", "center" or "right" for the first value and "top", "center", or "bottom" for the second value, or this vector contains two numeric values between 0 and 1 that specifies the x and y value of the left center of the text. By default, it is placed in the right bottom corner.
#' @export
#' @example ../examples/tm_scale_bar.R
tm_scale_bar <- function(breaks=NULL,
					     size=.5,
					     position=c("right", "bottom")) {
	g <- list(tm_scale=as.list(environment()))
	names(g$tm_scale) <- paste("scale", names(g$tm_scale), sep=".")
	class(g) <- "tmap"
	attr(g, "call") <- names(match.call(expand.dots = TRUE)[-1])
	g
}






#' Stacking of tmap elements
#' 
#' The plus operator allows you to stack \code{\link{tmap-element}s}, and groups of \code{\link{tmap-element}s}.
#' 
#' @param e1 first \code{\link{tmap-element}}
#' @param e2 second \code{\link{tmap-element}}
#' @seealso \code{\link{tmap-element}} and \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @export
"+.tmap" <- function(e1, e2) {
	g <- c(e1,e2)
	class(g) <- "tmap"
	g
}
