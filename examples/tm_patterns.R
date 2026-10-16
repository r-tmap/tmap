tm_patterns("hatch", angle = c(0, 45, 90, 135))
tm_patterns(c("dots", "stipple", "bricks"))

# as values of a scale
tm_shape(World) +
	tm_polygons(pattern = "economy",
		pattern.scale = tm_scale_categorical(values = tm_patterns(c("hatch", "dots", "bricks"))))
