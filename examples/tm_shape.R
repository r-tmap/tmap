tm_shape(World, crs = "+proj=ortho +lat_0=-10 +lon_0=-30") +
	tm_polygons()

tm_shape(World, crs = "+proj=robin", filter = World$continent=="Africa") +
	tm_polygons()
