tm_shape(World, crs = "auto") +
	tm_polygons()

tm_shape(World, crs = 3035, bb = "Europe") +
	tm_polygons()

tm_shape(World, crs = "+proj=robin", filter = World$continent=="Africa") +
	tm_polygons()
