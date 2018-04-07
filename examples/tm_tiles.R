data(World, metro)
tmap_mode("view")

tm_basemap(leaflet::providers$CartoDB.PositronNoLabels) +
tm_shape(World) +
	tm_polygons("HPI") +
tm_tiles(leaflet::providers$CartoDB.PositronOnlyLabels) +
tm_shape(metro) +
	tm_dots(col = "red")
