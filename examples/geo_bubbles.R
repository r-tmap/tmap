# World example
data(World)
geo_shape(World) +
	geo_fill() +
	geo_bubbles("pop_est", scale=2) +
	geo_theme_World("World population", legend.width=.4)

data(airports)
geo_shape(World) +
	geo_borders("white") +
	geo_fill("grey70") +
	geo_shape(airports) +
	geo_bubbles(size="natlscale", scale=.5, palette="Dark2") +
	geo_theme_World("Airport size")

# Europe example
data(Europe)

geo_shape(Europe) + geo_borders() + geo_bubbles(size=.75, col="red") + geo_theme("Countries")
geo_shape(Europe) + geo_borders() + geo_bubbles(size=.75, col=ifelse(Europe$name=="Isle of Man", "red", "blue")) + geo_theme("Find Isle of Man...")

geo_shape(Europe) +
	geo_borders() +
	geo_fill() +
	geo_bubbles("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe("GDP per country", legend.titles=c(bubble.col="Part of Europe"))

geo_shape(Europe) +
	geo_borders() +
	geo_fill() +
	geo_shape(airports) +
	geo_bubbles(size="natlscale", palette="Set1") +
	geo_shape(airports[airports$scalerank==2, ]) +
	geo_text("iata_code", ymod=-.01) +
	geo_theme_Europe("Airport size")

# Netherlands example
data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_prov) +
	geo_borders() +
	geo_fill("name") +
	geo_shape(NLD_muni) +
	geo_bubbles(size="pop", col="steelblue",style="kmeans") +
	geo_theme_NLD(title="Population", legend.digits=0, legend.config="bubble.size", legend.width=.4, bg.color="white")
