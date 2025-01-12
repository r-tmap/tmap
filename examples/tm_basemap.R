\dontrun{
if (requireNamespace("maptiles")) {
	# view mode
	current_mode = tmap_mode("view")
	tm_basemap("Stadia.StamenWatercolor") +
		tm_shape(World) +
		tm_polygons(
		  "HPI",
		  fill.scale = tm_scale(values = "reds"),
		  fill_alpha.scale = 0.5)

	tm_shape(World, crs = "+proj=eqearth") +
		tm_polygons(
			"HPI",
			fill.scale = tm_scale(values = "reds"),
			fill_alpha.scale = 0.5) +
	tm_basemap(NULL)

	# plot mode:
	tmap_mode("plot")
	tm_basemap() +
		tm_shape(World) +
		tm_polygons("HPI")

	tm_basemap("OpenTopoMap") +
		tm_shape(World) +
		tm_polygons(fill = NA, col = "black")

	tm_basemap("CartoDB.PositronNoLabels") +
	tm_shape(NLD_prov, crs = 4236) +
		tm_borders() +
		tm_facets_wrap("name") +
		tm_tiles("CartoDB.PositronOnlyLabels")

	# restore mode
	tmap_mode(current_mode)
}
}
