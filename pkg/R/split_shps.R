#' Divide into multiple shape objects
#'
#' Divide shape object into multiple objects.
#' 
#' @aliases split.SpatialPolygonsDataFrame
#' @param x shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#' }#' @param f factor to split \code{x}
#' @param drop unused factor levels are dropped
#' @param ... other arguments (not used)
#' @return List of shape objects.
#' @name split.SpatialPolygonsDataFrame
#' @rdname split_shapes
#' @method split SpatialPolygonsDataFrame
#' @export
split.SpatialPolygonsDataFrame <- function(x, f, drop=FALSE, ...) split_shape(x, f, drop=FALSE, ...)

#' @aliases split.SpatialPointsDataFrame
#' @name split.SpatialPointsDataFrame
#' @rdname split_shapes
#' @method split SpatialPointsDataFrame
#' @export
split.SpatialPointsDataFrame <- function(x, f, drop=FALSE, ...) split_shape(x, f, drop=FALSE, ...)

#' @aliases split.SpatialLinesDataFrame
#' @name split.SpatialLinesDataFrame
#' @rdname split_shapes
#' @method split SpatialLinesDataFrame
#' @export
split.SpatialLinesDataFrame <- function(x, f, drop=FALSE, ...) split_shape(x, f, drop=FALSE, ...)

split_shape <- function(x, f, drop=TRUE, ...) {
	if (!is.factor(f)) {
		warning("f is not a factor")
		f <- as.factor(f)
	}
	lev <- intersect(levels(f), f)
	
	xlist <- lapply(lev, function(l)x[which(f==l),])
	names(xlist) <- lev
	xlist
}