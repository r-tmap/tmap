current.mode <- tmap_mode("plot")

data(NLD_muni, World)

qtm(NLD_muni, borders = NULL) + tm_grid()

qtm(World, shape.projection = "robin", style = "natural") +
	tm_graticules(ticks = FALSE) +
	tm_layout(frame=FALSE)

tmap_mode(current.mode)
