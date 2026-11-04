# the area of the bubbles is proportional to the population (as with the default scale)
tm_shape(World) +
	tm_polygons() +
	tm_bubbles(size = "pop_est",
		size.scale = tm_scale_continuous_area())

# compensated for the underestimation of larger areas (Flannery, 1971)
tm_shape(World) +
	tm_polygons() +
	tm_bubbles(size = "pop_est",
		size.scale = tm_scale_continuous_area(perceptual = TRUE))

# bubbles that look like spheres: the volume is proportional to the population
tm_shape(World) +
	tm_polygons() +
	tm_bubbles(size = "pop_est",
		size.scale = tm_scale_continuous_volume(values = c(0, 2)))

# dot density: the number of dots is proportional to the population density
utrecht = NLD_dist[NLD_dist$province == "Utrecht", ]
utrecht$pop_dens = utrecht$population / utrecht$area # units: 1/km^2
tm_shape(utrecht) +
	tm_polygons(fill = "grey95", col = "grey60", lwd = 0.5,
		pattern = "stipple",
		pattern_size = 1.2,
		pattern_density = "pop_dens",
		pattern_density.scale = tm_scale_continuous_area(limits = c(0, 10000)),
		pattern_density.legend = tm_legend("Inhabitants per km2",
			dot.value = "1 dot \u2248 {n} inhabitants"))
