#' Divide into multiple shape objects
#'
#' Divide a shape object into multiple objects.
#' 
#' @aliases split.SpatialPolygons
#' @param x shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#' }
#' @param f factor to split \code{x}
#' @param drop unused factor levels are dropped
#' @param ... other arguments (not used)
#' @return List of shape objects.
#' @name split.SpatialPolygons
#' @rdname split_shapes
#' @method split SpatialPolygons
#' @export
split.SpatialPolygons <- function(x, f, drop=FALSE, ...) split_shape(x, f, drop=FALSE, ...)

#' @aliases split.SpatialPoints
#' @name split.SpatialPoints
#' @rdname split_shapes
#' @method split SpatialPoints
#' @export
split.SpatialPoints <- function(x, f, drop=FALSE, ...) split_shape(x, f, drop=FALSE, ...)

#' @aliases split.SpatialLines
#' @name split.SpatialLines
#' @rdname split_shapes
#' @method split SpatialLines
#' @export
split.SpatialLines <- function(x, f, drop=FALSE, ...) split_shape(x, f, drop=FALSE, ...)

split_shape <- function(x, f, drop=TRUE, ...) {
	if (!is.factor(f)) {
		warning("f is not a factor", call. = FALSE)
		f <- as.factor(f)
	}
	lev <- if (drop) {
		intersect(levels(f), f)	
	} else levels(f)
	xlist <- lapply(lev, function(l) {
		ids <- which(f==l)
		if (length(ids)==0L) NULL else x[ids,]
	})
	names(xlist) <- lev
	xlist
}