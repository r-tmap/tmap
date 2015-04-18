is_projected <- function(x) {
	isP <- if (inherits(x, "Spatial")) is.projected(x) else if (inherits(x, "Raster")) !couldBeLonLat(x, warnings=FALSE) else attr(x, "projected")
	if (is.na(isP)) {
		isP <- !maybe_longlat(attr(x, "bbox"))
	}
	isP
}

maybe_longlat <- function(bb) {
	(bb[1,1] >= -365 && bb[1,2] <= 365 && bb[2,1] >= -90.1 && bb[2,2] <= 90.1)
}