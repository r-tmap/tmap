file = system.file("tif/L7_ETMs.tif", package = "stars")

L7 = stars::read_stars(file)

# not working yet #819
tm_shape(L7) +
	tm_rgb()

tm_shape(L7) +
	tm_rgb(col = tm_mv_dim("band", 1:3))

tm_shape(L7) +
	tm_rgba(col = tm_mv_dim("band", 1:4))

library(stars)
L7_alt = split(L7, "band")

# not working yet #819
tm_shape(L7_alt) +
	tm_rgb()

tm_shape(L7_alt) +
	tm_rgb(col = tm_mv("X1", "X2", "X3"))

tm_shape(L7_alt) +
	tm_rgb(col = tm_mv(1:3))

L7_terra = terra::rast(file)

tm_shape(L7_terra) +
	tm_rgb(col = do.call(tm_mv, as.list(names(L7_terra)[1:3])))
