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
