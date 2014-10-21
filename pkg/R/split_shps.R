#' Divide into multiple shape objects
#'
#' Divide shape object into multiple objects
#' 
#' @param x shape object
#' @param f factor to split \code{x}
#' @param drop unused factor levels are dropped
#' @param other arguments (not used)
#' @return list of shape objects	
#' @name split.SpatialPolygonsDataFrame
#' @rdname Shapes
#' @export
split.SpatialPolygonsDataFrame <- function(x, f, drop=FALSE, ...) split_shape(x, f, drop=FALSE, ...)

#' @name split.SpatialPointsDataFrame
#' @rdname Shapes
#' @export
split.SpatialPointsDataFrame <- function(x, f, drop=FALSE, ...) split_shape(x, f, drop=FALSE, ...)

#' @name split.SpatialLinesDataFrame
#' @rdname Shapes
#' @export
split.SpatialLinesDataFrame <- function(x, f, drop=FALSE, ...) split_shape(x, f, drop=FALSE, ...)

split_shape <- function(x, f, drop=TRUE, ...) {
	if (!is.factor(f)) {
		warning("f is not a factor")
		f <- as.factor(f)
	}
	lev <- intersect(levels(f), f)
	
	xlist <- lapply(lev, function(l)x[f==l,])
	names(xlist) <- lev
	xlist
}