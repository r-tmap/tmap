has_raster <- function (x) {
	if (inherits(x, "stars")) 
		x = st_dimensions(x)
	!is.null(r <- attr(x, "raster")) && all(r$dimensions %in% 
												names(x))
}

get_raster = function (x) {
	attr(st_dimensions(x), "raster")
}

