if (requireNamespace("maptiles")) {
	tm_basemap() +
		tm_shape(World) +
		tm_polygons("HPI")
	
	tm_basemap("OpenTopoMap") +
		tm_shape(World) + 
		tm_polygons(fill = NA, col = "black")

	tm_basemap("CartoDB.PositronNoLabels") +
	tm_shape(NLD_prov, crs = 4236) +
		tm_borders() +
		tm_facets_wrap("name") +
		tm_tiles("CartoDB.PositronOnlyLabels")
	
}
