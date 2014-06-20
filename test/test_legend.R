data(Europe)
data(rivers)
data(cities)

geo_shape(Europe) +
	geo_fill("pop_est_dens") +
	geo_borders() +
geo_shape(rivers) +
	geo_lines(col="type", lwd="strokelwd", scale=4) +
geo_shape(cities) +
	geo_bubbles(size="pop_max", col="capital") +
	geo_text(text="name", cex="pop_max", root=3, scale=2, ymod=0.02, bg.alpha=0)
	

geo_shape(Europe) +
	geo_fill("pop_est_dens") +
	geo_borders() +
	geo_shape(rivers) +
	geo_lines(col="type", lwd="strokelwd", scale=4) +
	geo_shape(cities) +
	geo_bubbles(size="pop_max", col="capital") +
	geo_text(text="name", cex="pop_max", root=3, scale=2, ymod=0.02, bg.alpha=0) +
	geo_theme(legend.is.portrait=c(bubble.col=FALSE, bubble.size= FALSE, fill=FALSE,
								   line.lwd=FALSE, line.col=FALSE))