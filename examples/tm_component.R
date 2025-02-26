if (requireNamespace("ggplot2")) {
	library(ggplot2)
	p = ggplot(World, aes(x = gender, y = press, colour = continent)) +
		geom_point() +
		theme_bw()

	g = ggplotGrob(p)

	tm_shape(World) +
		tm_polygons(
			fill = "gender",
			fill.scale = tm_scale(values = "-cols4all.pu_gn_div")) +
		tm_component(g, height = 15, width = 20, position = tm_pos_in("left", "bottom"))
}
