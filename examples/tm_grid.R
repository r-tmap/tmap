data(NLD_muni, World, land)

qtm(NLD_muni, borders = NULL) + tm_grid()

land_eck4 <- set_projection(land, "eck4")
pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3")

tm_shape(land_eck4) +
	tm_raster("cover_cls", palette = pal8,legend.show = FALSE) +
tm_shape(World) +
	tm_borders() + 
tm_grid(projection = "longlat", labels.size = .6) + 
tm_layout(bg.color="lightblue3", earth.boundary = T, space.color = "white", inner.margins = c(.05, .05, .02, .02), frame=FALSE, outer.bg.color = "white")
