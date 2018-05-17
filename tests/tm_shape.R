library(tmap)

data(World)

tm_shape(World) +
	tm_polygons()
