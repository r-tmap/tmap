# load land data
data(land, World)

tm_shape(land) +
	tm_raster() +
	tm_facets_hstack()

tm_shape(land) +
	tm_raster("elevation", col.scale = tm_scale_continuous(values = terrain.colors(9))) +
tm_shape(World) +
	tm_borders()
