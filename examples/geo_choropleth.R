data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_muni) +
	geo_choropleth(col="pop", convert2density=TRUE, style="kmeans", total.area.km2=41543) +
	geo_borders("gray50") +
geo_shape(NLD_prov) +
	geo_borders("gray25", lwd=2) +
	geo_theme_NLD(title="Population (per km2)", legend.digits=0)

data(World)

(g <- geo_shape(World) +
 	geo_choropleth("pop_est_dens", style="fixed", breaks=c(0, 5, 20, 100, 250, 1000, 20000), palette="YlOrRd", auto.palette.mapping=FALSE) +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3") +
 	geo_theme_World(legend.profile="text", title="Population density per km2"))


(g <- geo_shape(World) +
 	geo_choropleth("income_grp", palette="-Greens") +
 	geo_borders() +
 	geo_bubblemap("pop_est") +
 	geo_text("iso_a3", cex="AREA3") +
 	geo_theme_World("Income classification", legend.bubble.size.title="Population"))

(g <- geo_shape(Europe) +
 	geo_choropleth("gdp_cap_est", style="kmeans") +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", scale=2) +
 	geo_theme_Europe("GDP per capita"))
