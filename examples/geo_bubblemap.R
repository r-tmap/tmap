# World example
data(World)
geo_shape(World) +
	geo_fill() +
 	geo_bubblemap("pop_est", scale=2) +
	geo_theme_World("World population", legend.width=.4)

# Europe example
data(Europe)
geo_shape(Europe) +
	geo_borders("grey50") +
	geo_fill("grey90") +
	geo_bubblemap("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe("GDP per country", legend.bubble.col.title="Part of Europe" , bg.color="white")


# Netherlands example
data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_prov) +
	geo_choropleth("name") +
geo_shape(NLD_muni) +
	geo_bubblemap(size="pop", style="kmeans") +
geo_shape(NLD_prov) +
	geo_borders("gray50") +
geo_theme_NLD(title="Population (per km2)", bg.color="white", legend.digits=0, legend.config="bubble.size", legend.width=.4)
