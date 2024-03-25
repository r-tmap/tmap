tm_shape(World, crs = "+proj=robin", filter = World$continent=="Africa") + 
	tm_polygons()