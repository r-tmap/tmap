
SA = World[World$continent == "South America", ]

# latlon coordinates (WGS84)
tm_shape(SA) +
	tm_polygons() +
	tm_graticules() +
	tm_crs(4326)

tm_list = lapply(c("global", "area", "distance", "shape"), FUN = function(property) {
	tm_shape(SA) +
		tm_polygons() +
		tm_graticules() +
		tm_crs(property = property)	+
	tm_title(property)
})

tmap_arrange(tm_list, nrow = 1)
