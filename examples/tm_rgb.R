require(stars)
file = system.file("tif/L7_ETMs.tif", package = "stars")

L7 = stars::read_stars(file)

tm_shape(L7) +
	tm_rgb()

\dontrun{
# the previous example was a shortcut of this call
tm_shape(L7) +
	tm_rgb(col = tm_mv_dim("band", 1:3))

# alternative format: using a stars dimension instead of attributes
L7_alt = split(L7, "band")
tm_shape(L7_alt) +
	tm_rgb()

# with attribute names
tm_shape(L7_alt) +
	tm_rgb(col = tm_mv("X1", "X2", "X3"))

# with attribute indices
tm_shape(L7_alt) +
	tm_rgb(col = tm_mv_shape_vars(1:3))

if (requireNamespace("terra")) {
	L7_terra = terra::rast(file)
	
	tm_shape(L7_terra) +
		tm_rgb()

	# with layer names
	tm_shape(L7_terra) +
		tm_rgb(tm_mv(names(L7_terra)[1:3]))

	# with layer indices
	tm_shape(L7_alt) +
		tm_rgb(col = tm_mv_shape_vars(1:3))
	
}
}
