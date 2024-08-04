data(World)
World$pop_est[31] = 1.2e9
World$pop_est[74] = 1e9

tm_shape(World) +
	tm_bubbles(size = "pop_est", scale = 3)

library(grid)

g = circleGrob(gp=gpar(lwd = 4))



tm_shape(World) +
	tm_bubbles(size = "pop_est", scale = 1,
			   shape = g)


tm_shape(World) +
	tm_bubbles(size = "pop_est", 
			   shape = list(tmap_icons("https://avatars.githubusercontent.com/u/65502380?s=200&v=4")))

tm_shape(World) +
	tm_bubbles(size = "pop_est", scale = 3) +
	tm_shape(World) +
	tm_bubbles(size = "pop_est", scale = 1,
			   shape = tmap_icons("https://avatars.githubusercontent.com/u/65502380?s=200&v=4"))

tm_shape(World) +
	tm_bubbles(size = "pop_est", scale = 3) +
	tm_shape(World) +
	tm_bubbles(size = "pop_est", scale = 1,
			   shape = list(g, g))

tm_shape(World) +
	tm_polygons(list("red", "blue"))


tm_shape(World) +
	tm_polygons(list("red", "blue"))


##### systematic testing tmapVars

# one value
tm_shape(World) +
	tm_polygons(fill = "pink")

tm_shape(World) +
	tm_polygons(lwd = 3)

# two values

tm_shape(World) +
	tm_polygons(fill = c("pink", "orange"))

tm_shape(World) +
	tm_polygons(lwd = c(3, 6))


tm_shape(World) +
	tm_polygons(fill = list("pink", "orange"))

tm_shape(World) +
	tm_polygons(lwd = list(3, 6))


# one variable
tm_shape(World) +
	tm_polygons(fill = "economy")

tm_shape(World) +
	tm_polygons(lwd = "HPI")


# two variables
tm_shape(World) +
	tm_polygons(fill = c("economy", "income_grp"))

tm_shape(World) +
	tm_polygons(lwd = c("HPI", "footprint"))

tm_shape(World) +
	tm_polygons(fill = list("economy", "income_grp"))

tm_shape(World) +
	tm_polygons(lwd = list("HPI", "footprint"))

# As Is
tm_shape(World) +
	tm_polygons(fill = I("red"))

tm_shape(World) +
	tm_polygons(fill = I(c("red", "blue")))

tm_shape(World) +
	tm_polygons(fill = list(I("red"), I("blue")))

# note: scale asis is same as any other scale
World$FILL1 = rainbow(177)
World$FILL2 = rev(rainbow(177))

tm_shape(World) +
	tm_polygons(fill = "FILL1", fill.scale = tm_scale_asis())

tm_shape(World) +
	tm_polygons(fill = c("FILL1", "FILL2"), fill.scale = tm_scale_asis())

# shape vars
tm_shape(World) +
	tm_polygons(fill = tm_shape_vars())

tm_shape(land) +
	tm_raster()


require(stars)
require(terra)
require(sf)
file = system.file("tif/L7_ETMs.tif", package = "stars")

L7 = stars::read_stars(file)

tm_shape(L7) +
	tm_rgb()

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


L7_terra = terra::rast(file)

tm_shape(L7_terra) +
	tm_rgb()

# with layer names
tm_shape(L7_terra) +
	tm_rgb(tm_mv(names(L7_terra)[1:3]))

# with layer indices
tm_shape(L7_alt) +
	tm_rgb(col = tm_mv_shape_vars(1:3))






