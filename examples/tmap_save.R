m = tm_shape(NLD_muni) +
	tm_polygons(
		fill = "population",
		fill.scale = tm_scale_intervals(style = "kmeans"),
		fill.legend = tm_legend("Population"),
		col = "grey50") +
tm_shape(NLD_prov) +
	tm_borders("grey25", lwd = 2) +
tm_scalebar(position = c("left", "bottom")) +
tm_compass(position = c("right", "bottom"))

# height interpreted in inches
tmap_save(m, file.path(tempdir(), "choropleth.png"), height = 7)

# height interpreted in pixels; a small image needs a smaller scale
tmap_save(m, file.path(tempdir(), "choropleth_small.png"), height = 300, scale = .3)

m2 = tm_shape(World, crs = "+proj=eqearth") +
	tm_polygons("well_being", fill.legend = tm_legend("Well-being"), id = "name")

# full HD image; asp = 0 means the map fills the whole image
tmap_save(m2, file.path(tempdir(), "World_map.png"), width = 1920, height = 1080, asp = 0)

\dontrun{
# save interactive map
tmap_save(m2, "World_map.html")
}
