current.mode <- tmap_mode("plot")

tm_shape(NLD_muni) + 
	tm_polygons() +
	tm_grid()

tm_shape(NLD_muni) + 
	tm_polygons() +
	tm_grid(projection = 4326)

tm_shape(NLD_muni) + 
	tm_polygons() +
	tm_grid(projection = 3035, labels.inside.frame = TRUE)


tm_shape(World) +
	tm_polygons() +
	tm_facets(by = "continent") +
	tm_grid(labels.inside.frame = TRUE)


tm_shape(NLD_muni) +
	tm_polygons() +
	tm_graticules()


tm_shape(NLD_muni) +
	tm_polygons() +
	tm_graticules(labels.pos = c("right", "top"))

## not working yet

data(NLD_muni, World)

tmap_arrange(
	qtm(NLD_muni, borders = NULL) + tm_grid(),
	qtm(NLD_muni, borders = NULL) + tm_graticules()
)

qtm(World, shape.projection = "+proj=robin", style = "natural") +
	tm_graticules(ticks = FALSE) +
	tm_layout(frame=FALSE)

tmap_mode(current.mode)
