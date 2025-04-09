# get all options
opt = tmap_options()

# print as a tree
if (requireNamespace("lobstr")) {
	lobstr::tree(opt)
}

# a fancy set of options:
tmap_options(
  bg.color = "steelblue",
  outer.bg.color = "salmon",
  frame.color = "purple3",
  frame.lwd = 5,
  compass.type = "8star",
  legend.bg.color = "gold",
  legend.position = tm_pos_in(pos.h = "left", pos.v = "top")
 )

if (requireNamespace("lobstr")) {
	lobstr::tree(
		tmap_options_diff()
	)
}

tm_shape(World) +
	tm_polygons("footprint")

tmap_options_save("fancy")

# the default style:
tmap_style("white")

tm_shape(World) +
	tm_polygons("footprint")

tmap_style("fancy")

tm_shape(World) +
	tm_polygons("footprint")

# reset all options
tmap_options_reset()
