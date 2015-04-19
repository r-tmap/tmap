data(World)
data(land)

tm_shape(land) +
	tm_raster("cover", max.categories = 20) +
tm_shape(World) +
	tm_borders()
	

tm_shape(land) +
	tm_raster(c("cover", "trees"), max.categories = 20) +
tm_shape(World) +
	tm_borders()


tm_shape(land) +
	tm_raster("cover", max.categories = 20) +
tm_facets(by="cover") +
tm_shape(World) +
	tm_borders()
