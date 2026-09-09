tm_shape(World) +
	tm_polygons(fill = "MAP_COLORS")

# fewer colors
tm_shape(World) +
	tm_polygons(fill = tm_map_colors(5))

# another coloring with the same number of colors
tm_shape(World) +
	tm_polygons(fill = tm_map_colors(5, permutation = 1))

# arguments of tmaptools::map_coloring() can be passed on
tm_shape(World) +
	tm_polygons(fill = tm_map_colors(5, algorithm = "greedy"))
