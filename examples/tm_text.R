# World example
data(World)
tm_shape(World) +
	tm_text("name", size="AREA")

# Europe example
data(Europe)
tm_shape(Europe) +
    tm_fill() +
    tm_borders() +
    tm_text("iso_a3", size="AREA", root=4, shadow = TRUE, fontcolor = "grey20", scale=2, 
			size.lowerbound = .1) +
tm_shape(Europe) +
	tm_text("name", size="AREA", root=4, scale=1, 
			ymod=-.04 * approx_areas(Europe, unit = "norm")^(1/4))
