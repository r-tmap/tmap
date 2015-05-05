#' Quick thematic map plot
#' 
#' This function is a convenient wrapper for drawing thematic maps quickly.
#' 
#' @param shp shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPixelsDataFrame]{SpatialPixels(DataFrame)}}}
#'  \item{\code{\link[raster:Raster-class]{RasterLayer, RasterStack, or RasterBrick}}}
#' }
#' @param fill either a color to fill the polygons, or name of the data variable in \code{shp} to draw a choropleth. Only applicable when \code{shp} is type 1 (see above).
#' @param bubble.size name of the data variable in \code{shp} for the bubblemap that specifies the sizes of the bubbles. If neither \code{bubble.size} nor \code{bubble.col} is specified, no bubblemap is drawn. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param bubble.col name of the data variable in \code{shp} for the bubblemap that specifies the colors of the bubbles. If neither \code{bubble.size} nor \code{bubble.col} is specified, no bubblemap is drawn. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param text Name of the data variable that contains the text labels. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param text.size Font size of the text labels. Either a constant value, or the name of a numeric data variable. Only applicable when \code{shp} is type 1, 2, or 3 (see above).
#' @param line.lwd either a line width or a name of the data variable that specifies the line width. Only applicable when \code{shp} is type 3 (see above).
#' @param line.col either a line color or a name of the data variable that specifies the line colors. Only applicable when \code{shp} is type 3 (see above).
#' @param raster either a color or a name of the data variable that specifices the raster colors. Only applicable when \code{shp} is type 4, 5, or 6 (see above).
#' @param borders color of the polygon borders. Use \code{NA} to omit the borders.
#' @param theme one of "World", "Europe", or "NLD"
#' @param scale numeric value that serves as the global scale parameter. All font sizes, bubble sizes, border widths, and line widths are controled by this value. The parameters \code{bubble.size}, \code{text.size}, and \code{line.lwd} can be scaled seperately with respectively \code{bubble.scale}, \code{text.scale}, and \code{line.scale}.
#' @param ... arguments passed on to the \code{tm_*} functions. If an argument name is not unique for a particular \code{tm_} function, then it should be prefixed with the function name without \code{"tm_"}. For instance, \code{style} is an argument of \code{\link{tm_fill}}, \code{\link{tm_bubbles}}, and \code{\link{tm_lines}}. Therefore, in order to define the \code{style} for a choropleth, its arugment name should be \code{fill.style}.  
#' @return \code{\link{tmap-element}}
#' @example ../examples/qtm.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @export
qtm <- function(shp, 
				fill="grey85",
				bubble.size=NULL,
				bubble.col=NULL,
				text=NULL,
				text.size=1,
				line.lwd=NULL,
				line.col=NULL,
				raster="grey70",
				borders="grey40",
				theme=NULL,
				scale=NA,
				...) {
	args <- list(...)
	shp_name <- deparse(substitute(shp))
	if (!inherits(shp, "SpatialPolygons")) {
		fill <- NULL
		borders <- NULL
		
		if (inherits(shp, "SpatialLines")) {
			if (missing(line.lwd)) line.lwd <- 1
			if (missing(line.col)) line.col <- "black"
		}
		if (inherits(shp, "SpatialPoints")) {
			if (missing(bubble.size)) bubble.size <- 1
			if (missing(bubble.col)) bubble.col <- "black"
		}
	}
	if (!inherits(shp, c("SpatialGrid", "SpatialPixels", "Raster"))) {
		raster <- NULL
	}
	
	dupl <- c("alpha", "auto.palette.mapping", "bg.color", "breaks", "col", "colorNA", "contrast", "labels", "lty", "lwd", "max.categories", "n", "palette", "scale", "style", "textNA", "text_separator", "text_less_than", "text_or_more", "xmod", "ymod", "title")
	
	fns <- c("tm_shape", "tm_fill", "tm_borders", "tm_bubbles", "tm_lines", "tm_raster", "tm_text", "tm_layout", "tm_grid", "tm_facets")
	fns_prefix <- c("shape", "fill", "borders", "bubble", "line", "raster", "text", "layout", "grid", "facets")
	
	skips <- list(tm_shape="shp", tm_fill="col", tm_borders="col", tm_bubbles=c("size", "col"), tm_lines=c("col", "lwd"), tm_raster="raster", tm_text=c("text", "size"), tm_layout="scale", tm_grid=NULL, tm_facets=NULL)
	
	
	args2 <- mapply(function(f, pre, sk, args, dupl){
		lnames <- setdiff(names(formals(f)), sk)
		isD <- lnames %in% dupl
		lnames2 <- lnames
		lnames2[isD] <- paste(pre, lnames2[isD], sep=".")
		arg <- args[intersect(names(args), lnames2)]
		if (length(arg)) names(arg) <- lnames[match(names(arg), lnames2)]
		arg
	}, fns, fns_prefix, skips, MoreArgs = list(args=args, dupl=dupl), SIMPLIFY=FALSE)
	
	g <- do.call("tm_shape", c(list(shp=shp), args2[["tm_shape"]]))
	g$tm_shape$shp_name <- shp_name
	if (!is.null(borders)) g <- g + do.call("tm_borders", c(list(col=borders), args2[["tm_borders"]]))
	if (!is.null(fill)) g <- g + do.call("tm_fill", c(list(col=fill), args2[["tm_fill"]]))

	if (!missing(line.lwd) || !missing(line.col)) g <- g + do.call("tm_lines", c(list(lwd=line.lwd, col=line.col), args2[["tm_lines"]]))
	
	if (!missing(bubble.size) || !missing(bubble.col)) {
		bubbleLst <- c(if (!missing(bubble.size)) list(size=bubble.size) else list(),
					   if (!missing(bubble.col)) list(col=bubble.col) else list())
		g <- g + do.call("tm_bubbles", c(bubbleLst, args2[["tm_bubbles"]]))	
	} 
	
	if (!missing(text)) g <- g + do.call("tm_text", c(list(text=text, size=text.size), args2[["tm_text"]]))

	if (!is.null(raster)) g <- g + do.call("tm_raster", c(list(col=raster), args2[["tm_raster"]]))
	
	if (length(args2[["tm_facets"]])) g <- g + do.call("tm_facets", args2[["tm_facets"]])

	scaleLst <- if (!missing(scale)) list(scale=scale) else list()
	if (missing(theme)) {
		g <- g + do.call("tm_layout", c(scaleLst, args2[["tm_layout"]]))	
	} else {
		if (!(theme %in% c("World", "Europe", "NLD"))) stop("Unknown theme")
		funct <- paste("tm_layout", theme, sep="_")
		g <- g + do.call(funct, c(scaleLst, args2[["tm_layout"]]))
	}
	
	g
}