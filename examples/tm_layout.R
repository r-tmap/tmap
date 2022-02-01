data(land, World)

tm_shape(land) +
	tm_raster("elevation", col.scale = tm_scale_intervals(breaks=c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  
														  values = terrain.colors(9), 
														  midpoint = NA),
			  col.legend = tm_legend(title="Elevation", position = tm_pos_in("left", "bottom"))) +
tm_shape(World, is.main = TRUE, crs = "+proj=eck4") +
	tm_borders("grey20") +
	#tm_graticules(labels.size = .5) +
	#tm_text("name", size="AREA") +
	#tm_compass(position = c(.65, .15), color.light = "grey90") +
	#tm_credits("Eckert IV projection", position = c("right", "BOTTOM")) +
	tm_style("classic") +
	tm_layout(bg.color="lightblue",
			  inner.margins=c(0, 0, .02, 0))
