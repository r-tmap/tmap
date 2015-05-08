data(World)
data(Europe)
data(NLD_muni)
data(NLD_prov)
data(metro)
data(land)

qtm(Europe)

qtm(World, fill = "economy", text="iso_a3", text.size = "AREA", fill.palette="-Blues", 
	theme = "World", fill.title="Economy")

tm_shape(World) +
	tm_fill("pop_est_dens", style="kmeans", palette="YlOrRd", title="Population per km2") +
	tm_borders() +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_layout_World()

metro$growth <- (metro$X2020 - metro$X2010) / (metro$X2010 * 10) * 100
tm_shape(World) +
	tm_fill("income_grp", style="kmeans", palette="-Blues", title="Income class") +
	tm_borders() +
	tm_text("iso_a3", size="AREA", scale=1.5) +
tm_shape(metro) +
	tm_bubbles("X2010", col = "growth", border.col = "black", 
			   border.alpha = .5, style="fixed", 
			   breaks=c(-Inf, seq(0, 6, by=2), Inf),
			   palette="-RdYlGn", contrast=1, 
			   title.size="Metro population (2010)", 
			   title.col="Annual growth rate (%)") + 
	tm_layout_World(title="Income and urbanization")

tm_shape(Europe) +
	tm_borders() +
	tm_fill() +
tm_shape(metro) +
	tm_bubbles(size="X2010", col="purple", size.lim=c(0, 1.2e7), 
		title.size="Metro population") +
	tm_text("name", size="X2010", scale=2, root=3, ymod=-.015 , bg.alpha=0) +
	tm_layout_Europe()

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", free.coords=TRUE, drop.shapes=TRUE) +
	tm_layout(legend.show = FALSE)

pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3")
tm_shape(land, ylim = c(-88,88), relative=FALSE) +
	tm_raster("cover_cls", palette = pal8, title="Global Land Cover") +
tm_shape(World) +
	tm_borders() +
	tm_layout_World(inner.margins=0, 
					legend.position = c("left","bottom"), 
					legend.bg.color = "white", legend.bg.alpha=.2, 
					legend.frame="gray50", legend.width=.2)


tm_shape(land, ylim = c(-88,88), relative=FALSE) +
	tm_raster("elevation", n=10, style="kmeans", palette = terrain.colors(10), 
		title="Elevation") +
tm_shape(World) +
	tm_borders() +
	tm_layout(inner.margins=0, legend.position = c(.02, .1), 
			  bg.color="lightblue")
