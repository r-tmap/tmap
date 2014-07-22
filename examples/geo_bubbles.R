# World example
data(World)

geo_shape(World) +
	geo_fill() +
	geo_bubbles("pop_est") +
	geo_theme_World("World population")

# Europe example
data(Europe)
data(cities)

geo_shape(Europe) +
	geo_borders() +
	geo_fill() +
geo_shape(cities) +
	geo_text("name", cex="pop_max", scale=2, root=3, ymod=-.015, bg.alpha=0) +
	geo_bubbles(size="pop_max", col="capital", size.lim=c(0, 2e7)) +
	geo_theme_Europe("Metropolitan population", legend.titles=c(bubble.col="Capital"))


# Netherlands example
data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_prov) +
	geo_borders() +
	geo_fill("name", palette="Pastel1") +
geo_shape(NLD_muni) +
	geo_bubbles(size="population", col="steelblue",style="kmeans") +
	geo_theme_NLD(title="Population", legend.digits=0, legend.config="bubble.size", legend.width=.4, bg.color="white")
