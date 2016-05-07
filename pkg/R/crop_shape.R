#' Crop shape object
#' 
#' Crop a shape object (from class \code{\link[sp:Spatial]{Spatial-class}} or \code{\link[raster:Raster-class]{Raster}}).
#' 
#' This function is a wrapper around \code{\link[raster:crop]{crop}} from the raster package. It does two things in addition: \code{\link[sp:SpatialGrid]{SpatialGrid}} objects are allowed, and if \code{y} is a shape object with a different projection, it is temporarily reprojected in order to obtain a bounding box by which \code{x} is cropped.
#' 
#' @param x shape object, i.e. an object from class \code{\link[sp:Spatial]{Spatial-class}} or \code{\link[raster:Raster-class]{Raster}}
#' @param y bounding box (2 by 2 matrix), an \code{\link[raster:extent]{extent}}, or a shape object from which the bounding box is extracted.
#' @param ... arguments passed on to \code{\link[raster:crop]{crop}}
#' @export
#' @seealso \code{\link{bb}}
#' @example ../examples/crop_shape.R
crop_shape <- function(x, y, ...) {
	xname <- deparse(substitute(x))
	yname <- deparse(substitute(y))

	if (!inherits(x, c("Spatial", "Raster"))) stop(xname, " is not a spatial/raster object.", call.=FALSE)
	
	px <- get_projection(x)
	
	if (inherits(y, c("Spatial", "Raster"))) {
		py <- get_projection(y)
		
		# align projections
		if (!is.na(px) && !is.na(py)) {
			if (px!=py) {
				y <- set_projection(y, projection = px)
			}
		}
		y <- bb(y, as.extent = TRUE)
	} else {
		if (!((is.matrix(y) || is.vector(y)) && length(y)==4) &&
			!inherits(y, "Extent")) {
			stop(yname, " is not a bounding box or an Extent object.", call.=FALSE)
		}
	}
	
	# 
	isSG <- inherits(x, "SpatialGrid")
	hasData <- ("data" %in% slotNames(x))
	
	if (isSG) x <- brick(x)
	
	x2 <- crop(x, y, ...)
	
	if (isSG) {
		if (hasData) data <- get_raster_data(x2)
		x2 <- as(x2, "SpatialGrid")
		if (hasData) x2 <- SpatialGridDataFrame(x2, data=data)
	}
	x2
}
