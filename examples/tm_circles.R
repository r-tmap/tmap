## Three concentric layers of geographic circles at different administrative
## levels, each with a fixed radius that corresponds to a real-world distance.
## Because the radius is in meters the circles scale with zoom in view mode.
tm_shape(NLD_prov) +
	tm_circles(size = 5000, fill = "#0033ff", col = NULL) +
tm_shape(NLD_muni) +
	tm_circles(size = 2000, fill = "#99dd99", col = NULL) +
tm_shape(NLD_dist) +
	tm_circles(size = 1000, fill = "#ff8833", col = NULL)

## Use a units object — any linear unit is accepted and converted to meters
NLD_prov$one_mile <- units::as_units(1:12, "mi")
tm_shape(NLD_prov) +
	tm_circles(size = "one_mile", size.scale = tm_scale_asis())
