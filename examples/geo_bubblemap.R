# World example
data(World)
geo_shape(World) +
	geo_fill() +
 	geo_bubblemap("pop_est", scale=2) +
	geo_theme_World("World population", legend.width=.4)

# Europe example
data(Europe)
geo_shape(Europe) +
	geo_borders() +
	geo_fill() +
	geo_bubblemap("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe("GDP per country", legend.bubble.col.title="Part of Europe")


# Netherlands example
data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_prov) +
	geo_borders() +
	geo_choropleth("name") +
geo_shape(NLD_muni) +
	geo_bubblemap(size="pop", col="steelblue",style="kmeans") +
geo_theme_NLD(title="Population", legend.digits=0, legend.config="bubble.size", legend.width=.4, bg.color="white")
