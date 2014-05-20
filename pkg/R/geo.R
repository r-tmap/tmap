#' Quick map plot
#' 
#' This function is a convenient wrapper for drawing quick cartographic maps.
#' 
#' @param shp shape object. For \code{\link{geo_choropleth}} and \code{\link{geo_bubblemap}}, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} or a \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} is requied. \code{\link[sp:SpatialPoints]{SpatialPoints}} and \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} are only used for \code{\link{geo_bubblemap}} and \code{\link{geo_bubbles}}.
#' @param choro.fill name of the data variable in \code{shp} for the choropleth. If not specified, no choropleth is drawn.
#' @param bubble.size name of the data variable in \code{shp} for the bubblemap that specifies the sizes of the bubbles. If neither \code{bubble.size} nor \code{bubble.col} is specified, no bubblemap is drawn.
#' @param bubble.col name of the data variable in \code{shp} for the bubblemap that specifies the colors of the bubbles. If neither \code{bubble.size} nor \code{bubble.col} is specified, no bubblemap is drawn.
#' @param borders color of the polygon borders. Use \code{NA} to omit the borders.
#' @param fill specifies the fill colors in case \code{choro.fill} is not specified
#' @param text name of a variable in \code{shp} that contain text labels
#' @param theme one of "World", "Europe", or "NLD"
#' @param ... parameters passed on to the \code{geo_*} functions.
#' @return \code{\link{geo-element}}
#' @example ../examples/geo.R
#' @export
geo <- function(shp, 
				fill="grey90",
				bubble.size=NULL,
				bubble.col=NULL,
				borders="grey40",
				text=NULL,
				theme=NULL,
				...) {
	args <- list(...)
	shapeargs <- args[intersect(names(args), names(geo_shape()[[1]]))]
	fillargs <- args[setdiff(intersect(names(args), names(geo_fill()[[1]])), "col")]
	bubblenames <- names(geo_bubbles()[[1]])
	bubblenames[bubblenames=="bubble.scale"] <- "scale"
	bubblenames[bubblenames=="bubble.border"] <- "border"
	
	bubbleargs <- args[setdiff(intersect(names(args), bubblenames), c("col", "size"))]
	borderargs <- args[setdiff(intersect(names(args), names(geo_borders()[[1]])), "col")]
	textnames <- names(geo_text("")[[1]])[-1]
	textnames <- substr(textnames, 6, nchar(textnames))
	textargs <- args[intersect(names(args), textnames)]
	themeargs <- args[intersect(names(args), names(geo_theme()[[1]]))]
	gridargs <- args[intersect(names(args), names(geo_grid()[[1]]))]
	
	g <- do.call("geo_shape", c(list(shp=shp), shapeargs)) +
		do.call("geo_borders", c(list(col=borders), borderargs))
	g <- g + do.call("geo_fill", c(list(col=fill), fillargs))
	if (!missing(bubble.size) || !missing(bubble.col)) g <- g + do.call("geo_bubbles", c(list(size=bubble.size, col=bubble.col), bubbleargs))
	
	if (!missing(text)) g <- g + do.call("geo_text", c(list(text=text), textargs))
	
	if (missing(theme)) {
		if (length(themeargs)) g <- g + do.call("geo_theme", themeargs)	
	} else {
		if (!(theme %in% c("World", "Europe", "NLD"))) stop("Unknown theme")
		funct <- paste("geo_theme", theme, sep="_")
		g <- g + do.call(funct, themeargs)
	}
	
	g
}