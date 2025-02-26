bb = tmaptools::bb(NLD_prov[NLD_prov$name == "Utrecht",], ext = 1.05)

tm_shape(NLD_dist) +
	tm_polygons(
		fill = "dwelling_value",
		fill.scale = tm_scale_continuous_pseudo_log(values = "-cols4all.pu_gn_div"),
		col = NULL) +
tm_shape(NLD_muni) +
	tm_borders(col = "black", lwd = 0.5) +
tm_shape(NLD_prov) +
	tm_borders(col = "black", lwd = 1.5) +
tm_inset(bbox = bb, height = 15, width = 15)
