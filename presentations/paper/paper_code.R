

data(NLD_muni)
data(NLD_prov)

tm_shape(NLD_muni) +
	tm_borders(alpha = .5) +
	tm_fill("population", convert2density = TRUE, style= "kmeans") +
tm_shape(NLD_prov) +
	tm_borders(lwd=3) +
	tm_text("name", shadow=TRUE, bg.color="white", bg.alpha=.25) +
	tm_layout("Population per km2", draw.frame=FALSE, bg.color="white", inner.margins=c(.02, .1, .02, .02)) +
	tm_layout(outer.margins=0, asp=0, scale=.9)

data(Europe)
data(rivers)

tm_shape(Europe) +
	tm_fill("darkolivegreen3") +
tm_shape(rivers) +
	tm_lines(col="navy", lwd="scalerank", scale=2) +
	tm_layout("Rivers of Europe", legend.show=FALSE)


data(World)
data(cities)

tm_shape(World) +
	tm_fill() +
	tm_shape(cities) +
	tm_bubbles("pop_max", scale=.5) +
	tm_layout_World("Cities of the World")