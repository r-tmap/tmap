#' Quick thematic map plot
#' 
#' This function is a convenient wrapper for drawing thematic maps quickly.
#' 
#' @param shp shape object. For \code{\link{tm_fill}} and \code{\link{tm_bubbles}}, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} or a \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} is requied. 
#' \code{\link[sp:SpatialPoints]{SpatialPoints}} and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} are only used for \code{\link{tm_bubbles}}.
#' @param fill either a color to fill the polygons, or name of the data variable in \code{shp} to draw a choropleth.
#' @param bubble.size name of the data variable in \code{shp} for the bubblemap that specifies the sizes of the bubbles. If neither \code{bubble.size} nor \code{bubble.col} is specified, no bubblemap is drawn.
#' @param bubble.col name of the data variable in \code{shp} for the bubblemap that specifies the colors of the bubbles. If neither \code{bubble.size} nor \code{bubble.col} is specified, no bubblemap is drawn.
#' @param text Name of the data variable that contains the text labels.
#' @param text.cex Font size of the text labels. Either a constant value, or the name of a numeric data variable.
#' @param line.lwd either a line width or a name of the data variable that specifies the line width. Only applicable if \code{shp} is a \code{\link[sp:SpatialLines]{SpatialLines}} or \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}}.
#' @param line.col either a line color or a name of the data variable that specifies the line colors. Only applicable if \code{shp} is a \code{\link[sp:SpatialLines]{SpatialLines}} or \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}}.
#' @param borders color of the polygon borders. Use \code{NA} to omit the borders.
#' @param theme one of "World", "Europe", or "NLD"
#' @param scale numeric value that serves as the global scale parameter. All font sizes, bubble sizes, border widths, and line widths are controled by this value. The parameters \code{bubble.size}, \code{text.cex}, and \code{line.lwd} can be scaled seperately with respectively \code{bubble.scale}, \code{text.scale}, and \code{line.scale}.
#' @param ... parameters passed on to the \code{tm_*} functions.
#' @return \code{\link{tmap-element}}
#' @example ../examples/qtm.R
#' @seealso \href{../doc/tmap-nutshell.html}{\code{vignette("tmap-nutshell")}}
#' @export
qtm <- function(shp, 
				fill="grey85",
				bubble.size=NULL,
				bubble.col=NULL,
				text=NULL,
				text.cex=1,
				line.lwd=NULL,
				line.col=NULL,
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
	
	dupl <- c("alpha", "auto.palette.mapping", "bg.color", "breaks", "col", "colorNA", "contrast", "labels", "lty", "lwd", "max.categories", "n", "palette", "scale", "style", "textNA", "xmod", "ymod")
	
	fns <- c("tm_shape", "tm_fill", "tm_borders", "tm_bubbles", "tm_lines", "tm_text", "tm_layout", "tm_grid", "tm_facets")
	fns_prefix <- c("shape", "fill", "borders", "bubble", "line", "text", "layout", "grid", "facets")
	
	skips <- list(tm_shape="shp", tm_fill="col", tm_borders="col", tm_bubbles=c("size", "col"), tm_lines=c("col", "lwd"), tm_text=c("text", "cex"), tm_layout="scale", tm_grid=NULL, tm_facets=NULL)
	
	
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
	
	if (!missing(text)) g <- g + do.call("tm_text", c(list(text=text, cex=text.cex), args2[["tm_text"]]))
	
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