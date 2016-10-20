data(World, metro)

World$color <- map_coloring(World, palette="Pastel2")
qtm(World, fill = "color")

# map_coloring used indirectly: qtm(World, fill = "MAP_COLORS")

data(NLD_prov, NLD_muni)
tm_shape(NLD_prov) + 
	tm_fill("name", legend.show = FALSE) + 
tm_shape(NLD_muni) + 
	tm_polygons("MAP_COLORS", palette="Greys", alpha = .25) + 
tm_shape(NLD_prov) + 
	tm_borders(lwd=2) +
	tm_text("name", shadow=TRUE) +
tm_format_NLD(title="Dutch provinces and\nmunicipalities", bg.color="white")
