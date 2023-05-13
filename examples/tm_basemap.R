tm_basemap() +
	tm_shape(World) +
	tm_polygons("HPI")

tm_basemap("OpenTopoMap") +
	tm_shape(World) + 
	tm_polygons(fill = NA, col = "black")
