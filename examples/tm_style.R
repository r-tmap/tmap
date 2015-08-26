# create world map
data(World, land)
tm <- tm_shape(land, ylim = c(-88,88)) +
    tm_raster("elevation", n=10, style="kmeans", palette = terrain.colors(10), 
        title="Elevation") +
    tm_shape(World) +
    tm_borders() +
	tm_grid(n.x = 18) +
    tm_layout(inner.margins=0, legend.position = c("left", "bottom"), legend.frame = TRUE, 
        bg.color="lightblue", legend.bg.color="lightblue")

# plot in default style
tm + tm_style()

# plot in classic style
tm + tm_style_classic()

# classic choropleth
data(NLD_muni, NLD_prov)
tm_shape(NLD_muni) +
	tm_fill(col="population", convert2density=TRUE, 
			style="kmeans", title="Population (per km2)", legend.hist=FALSE) +
	tm_borders("black", alpha=.5) + 
	tm_shape(NLD_prov) +
	tm_borders("grey25", lwd=2) +
	tm_layout_NLD(inner.margins = c(.02, .15, .06, .15)) + 
	tm_scale_bar(position = c("left", "bottom")) +
	tm_compass(position=c("right", "bottom")) + 
	tm_style_classic()
