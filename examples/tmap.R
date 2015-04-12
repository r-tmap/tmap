data(World)
data(Europe)
data(NLD_muni)
data(NLD_prov)
data(metro)

qtm(Europe)

qtm(World, fill = "economy", text="iso_a3", text.size = "AREA", fill.palette="-Blues", 
	theme = "World", title="Economy")

tm_shape(World) +
	tm_fill("pop_est_dens", style="kmeans", palette="YlOrRd") +
	tm_borders() +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_layout_World(title="Population per km2")

metro$growth <- (metro$X2020 - metro$X2010) / (metro$X2010 * 10) * 100
tm_shape(World) +
	tm_fill("income_grp", style="kmeans", palette="-Blues") +
	tm_borders() +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_shape(metro) +
	tm_bubbles("X2010", col = "growth", border.col = "black", border.alpha = .5, style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf) ,palette="-RdYlGn", contrast=1) + 
	tm_layout_World(title="Income and urbanization", legend.titles=c(fill="Income class", bubble.size="Metro population (2010)", bubble.col="Annual growth rate (%)"), legend.hist.show=FALSE)



tm_shape(Europe) +
	tm_borders() +
	tm_fill() +
	tm_shape(metro) +
	tm_bubbles(size="X2010", col="purple", size.lim=c(0, 1.2e7)) +
	tm_text("name", size="X2010", scale=2, root=3, ymod=-.015 , bg.alpha=0) +
	tm_layout_Europe("Metro population", legend.titles=c(bubble.col="Capital"))

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", free.coords=TRUE, drop.shapes=TRUE) +
	tm_layout(legend.show = FALSE)
