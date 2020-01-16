current.mode <- tmap_mode("plot")

data(NLD_muni, World)

tmap_arrange(
	qtm(NLD_muni, borders = NULL) + tm_grid(),
	qtm(NLD_muni, borders = NULL) + tm_graticules()
)

qtm(World, shape.projection = "+proj=robin", style = "natural") +
	tm_graticules(ticks = FALSE) +
	tm_layout(frame=FALSE)

tmap_mode(current.mode)
