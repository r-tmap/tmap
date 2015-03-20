data(World)
data(Europe)
data(NLD_muni)
data(NLD_prov)

qtm(Europe)

qtm(World, fill = "economy", text="iso_a3", text.cex = "AREA", fill.palette="-Blues", theme = "World", title="Economy")

tm_shape(World) +
	tm_fill("pop_est_dens", style="kmeans", palette="YlOrRd") +
	tm_borders() +
	tm_text("iso_a3", cex="AREA", cex.lowerbound=.4) +
	tm_layout_World(title="Population density per km2")

tm_shape(Europe) +
	tm_borders() +
	tm_fill() +
tm_shape(cities) +
	tm_text("name", cex="pop_max", scale=2, root=3, ymod=-.015, bg.alpha=0) +
	tm_bubbles(size="pop_max", col="capital", size.lim=c(0, 2e7)) +
	tm_layout_Europe("Metropolitan population", legend.titles=c(bubble.col="Capital"))

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", free.coords=TRUE, split=TRUE) +
	tm_layout(legend.show = FALSE)