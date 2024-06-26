downsample_stars = function(x, max.raster) {
	k = length(dim(x))
	xy_dim = get_xy_dim(x)
	
	fact = round(sqrt(prod(xy_dim) / max.raster) - 1)
	
	n = dim(x)
	n[] = 0L
	n[names(xy_dim)] = fact
	
	if (inherits(x, "stars_proxy") || (fact > 0)) {
		y = stars::st_downsample(x, n)
		message("stars object downsampled to ", paste(get_xy_dim(y), collapse = " by "), " cells.")
	} else {
		y = x
	}
	y
}

downsample_SpatRaster = function(x, max.raster) {
	xy_dim = dim(x)[1:2]
	
	downsample = prod(xy_dim) > max.raster
	
	y = if (downsample) {
		terra::spatSample(x, max.raster, method="regular", as.raster=TRUE)
	} else x
	
	
	if (downsample) message("SpatRaster object downsampled to ", paste(dim(y)[1:2], collapse = " by "), " cells.")
	
	y
}
