data(World, metro)
tmap_mode("view")

tm_basemap(leaflet::providers$CartoDB.PositronNoLabels, group = "CartoDB basemap") +
tm_shape(World) +
	tm_polygons("HPI", group = "Countries") +
tm_tiles(leaflet::providers$CartoDB.PositronOnlyLabels, group = "CartoDB labels") +
tm_shape(metro) +
	tm_dots(col = "red", group = "Metropolitan areas") +
tm_view(set.view = )
