tm_shape(World) +
  tm_polygons() +
tm_layout(
  bg.color = "steelblue",
  outer.bg.color = "gold",
  frame.lwd = 3,
  inner.margins = 0)

tm_shape(World) +
  tm_polygons(fill = "HPI") +
tm_style("classic")

tm_shape(World) +
	tm_polygons(fill = "HPI") +
	tm_style("cobalt")
