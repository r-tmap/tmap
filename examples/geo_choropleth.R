# World examples
data(World)
geo_shape(World) +
 	geo_choropleth("pop_est_dens", style="kmeans", palette="YlOrRd", auto.palette.mapping=FALSE) +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", cex.lowerbound=.4, bg.alpha=0) +
 	geo_theme_World(legend.profile="text", title="Population density per km2", title.bg.color=TRUE)

geo_shape(World) +
 	geo_choropleth("income_grp", palette="Set2") +
 	geo_borders() +
 	geo_bubblemap("pop_est", col="blue") +
 #	geo_text("iso_a3", cex="AREA3") +
 	geo_theme_World("Income classification", legend.bubble.size.title="Population")

# Europe example
data(Europe)
geo_shape(Europe) +
 	geo_choropleth("gdp_cap_est", style="kmeans") +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA4", scale=2, bg.alpha=0) +
 	geo_theme_Europe("GDP per capita")

# Netherlands example
data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_muni) +
	geo_choropleth(col="pop", convert2density=TRUE, style="kmeans", total.area.km2=41543) +
	geo_borders() +
	geo_shape(NLD_prov) +
	geo_borders(, lwd=2) +
	geo_theme_NLD(title="Population (per km2)", legend.digits=0)
