data(Europe, land)

land_europe <- crop_shape(land, Europe)

tm_shape(land_europe) + 
	tm_raster("trees", breaks=seq(0, 100, by=20), title="Tree Cover") +
tm_shape(Europe, is.master = TRUE) +
	tm_borders() +
tm_format_Europe()
