\dontrun{
# view mode
current_mode = tmap_mode("view")
tm_basemap("Stadia.StamenWatercolor") +
	tm_shape(World) +
	tm_polygons(
	  "HPI",
	  fill.scale = tm_scale(values = "brewer.reds"),
	  fill_alpha = 0.5)

tm_shape(World, crs = "+proj=eqearth") +
	tm_polygons(
		"HPI",
		fill.scale = tm_scale(values = "brewer.reds"),
		fill_alpha = 0.5) +
tm_basemap(NULL)

# plot mode:
tmap_mode("plot")
tm_basemap() +
	tm_shape(World) +
	tm_polygons("HPI")

tm_basemap("OpenTopoMap") +
	tm_shape(World) +
	tm_polygons(fill = NA, col = "black")

tm_basemap("Esri.WorldGrayCanvas") +
tm_shape(NLD_prov, crs = 4326) +
	tm_borders() +
	tm_facets_wrap("name")

# CARTO basemaps require an API key
tm_basemap("CartoDB.PositronNoLabels", api = "YOUR_KEY") +
tm_shape(NLD_prov, crs = 4326) +
	tm_borders() +
	tm_tiles("CartoDB.PositronOnlyLabels", api = "YOUR_KEY")

# restore mode
tmap_mode(current_mode)
}
