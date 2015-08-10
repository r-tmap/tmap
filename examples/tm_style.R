data(land)
data(World)
tm <- tm_shape(land, ylim = c(-88,88), relative=FALSE) +
    tm_raster("elevation", n=10, style="kmeans", palette = terrain.colors(10), 
        title="Elevation") +
    tm_shape(World) +
    tm_borders() +
	tm_grid(n.x = 18) +
	tm_compass() +
    tm_layout(inner.margins=0, legend.position = c("left", "bottom"), legend.frame = TRUE, 
        bg.color="lightblue", legend.bg.color="lightblue", legend.width=.13)

tm + tm_style()
tm + tm_style_classic()


data(NLD_muni, NLD_prov)
tm_shape(NLD_muni) +
	tm_fill(col="population", convert2density=TRUE, 
			style="kmeans", title="Population (per km2)", legend.hist=FALSE) +
	tm_borders("grey25", alpha=.5) + 
	tm_shape(NLD_prov) +
	tm_borders("grey40", lwd=2) +
	tm_layout_NLD(bg.color="white", draw.frame = FALSE) + 
	tm_scale_bar(position = c("left", "bottom")) +
	tm_compass(type = "8star") + 
	tm_style_classic()

