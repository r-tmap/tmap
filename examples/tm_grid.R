current.mode <- tmap_mode("plot")

data(NLD_muni, Europe, World)

qtm(NLD_muni, borders = NULL) + tm_grid()

qtm(Europe) + tm_grid(projection="longlat")

qtm(World, shape.projection="wintri", style="natural") + tm_grid(y=c(-60, -40, -23.4, -20, 0, 20, 23.4, 40, 60), projection = "longlat")

tmap_mode(current.mode)
