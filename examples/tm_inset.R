## map
bb = tmaptools::bb(NLD_prov[NLD_prov$name == "Utrecht",], ext = 1.05)

bb_Randstad =
  sf::st_bbox(c(xmin = 120000, xmax = 150000, ymin = 460000, ymax = 500000), crs = 28992)

tm_shape(NLD_dist) +
	tm_polygons(
		fill = "dwelling_value",
		fill.scale = tm_scale_continuous_pseudo_log(values = "-cols4all.pu_gn_div"),
		col = NULL) +
tm_shape(NLD_muni) +
	tm_borders(col = "black", lwd = 0.5) +
tm_shape(NLD_prov) +
	tm_borders(col = "black", lwd = 1.5) +
tm_inset(bb_Randstad, height = 12, width = 12, position = c("left", "top")) +
	tm_compass(position = c("left", "top"), )

## ggplot2
if (requireNamespace("ggplot2")) {
	library(ggplot2)
	p = ggplot(World, aes(x = gender, y = press, colour = continent)) +
		geom_point() +
		theme_bw()

	tm_shape(World) +
		tm_polygons(
			fill = "gender",
			fill.scale = tm_scale(values = "-cols4all.pu_gn_div")) +
		tm_inset(p, height = 15, width = 20, position = tm_pos_in("left", "bottom"))
}
