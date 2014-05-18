# World example
data(World)
geo_shape(World) +
	geo_fill() +
	geo_bubbles("pop_est", scale=2) +
	geo_theme_World("World population", legend.width=.4)

# Europe example
data(Europe)

geo_shape(Europe) + geo_borders() + geo_bubbles(size=.75, col="red") + geo_theme("Countries")
geo_shape(Europe) + geo_borders() + geo_bubbles(size=.75, col=ifelse(Europe$name=="Isle of Man", "red", "blue")) + geo_theme("Find Isle of Man...")

geo_shape(Europe) +
	geo_borders() +
	geo_fill() +
	geo_bubbles("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe("GDP per country", legend.bubble.col.title="Part of Europe")


# Netherlands example
data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_prov) +
	geo_borders() +
	geo_fill("name") +
	geo_shape(NLD_muni) +
	geo_bubbles(size="pop", col="steelblue",style="kmeans") +
	geo_theme_NLD(title="Population", legend.digits=0, legend.config="bubble.size", legend.width=.4, bg.color="white")

geo_shape(NLD_muni) +
	geo_borders() +
	geo_shape(NLD_prov) +
	geo_bubbles(col="name", size=2)