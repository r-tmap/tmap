data(Europe)
data(World)
data(NLD_muni)
data(NLD_prov)
data(NLD_ageGroups)

NLD_muni$gender <- (NLD_muni$men/NLD_muni$pop -.5) / sqrt(NLD_muni$pop) * 10000
World$pop_est_km <- calc_densities(World, "pop_est", total.area.km2=148940000)

# head(World[order(World$pop_est_km, decreasing=TRUE), c("name", "pop_est", "pop_est_km")], 100)
# areas <- getAreas(World)
# World$area <- areas * (148940000 / sum(areas))
# head(World[order(World$area, decreasing=TRUE), c("name", "area")], 100)


### Pretty example for the Netherlands
geo_shape(NLD_muni) +
geo_choropleth(col="pop", convert2density=TRUE, style="kmeans", total.area.km2=41543) +
geo_borders() +
geo_shape(NLD_prov) +
geo_borders(lwd=2) +
geo_text("name", bg.color="white", bg.alpha=150) +
geo_theme_NLD(title="Population (per km2)", legend.digits=0)


(g <- geo_shape(World) +
 	geo_choropleth(c("gdp_cap_est"), style="kmeans", palette="RdYlGn") +
 	geo_borders() +
 	geo_theme_World())


(g <- geo_shape(World) +
 	geo_choropleth("pop_est_km", style="fixed", breaks=c(0, 5, 20, 100, 250, 1000, 30000), palette="YlOrRd", auto.palette.mapping=FALSE) +
 	geo_borders() +
 	geo_theme_World(type.legend.plot="none", title="Population density per km2", title.cex=1.8,
 					title.position=c("center", "top"), frame.margins=c(0,.05,.1,.05)))



(g <- geo_shape(World) +
 	geo_choropleth("income_grp") +
 	geo_borders() +
 	geo_bubblemap("pop_est") +
 	geo_theme(legend.position=c("left", "bottom"), legend.plot.size=c(.2, .2), legend.cex=0.6, draw.frame=FALSE))

(g <- geo_shape(Europe) +
	geo_choropleth("gdp_cap_est", style="kmeans") +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA2", scale=4) +
 	geo_theme(legend.position=c("left", "top"), legend.plot.size=c(.3, .25), 
 			  legend.cex=0.6, draw.frame=TRUE,
 			  frame.margins=c(0,.2,0,0)))


(g <- geo_shape(NLD_prov) +
 	geo_choropleth("pop", style="kmeans", convert2density=TRUE) + geo_theme(draw.frame=TRUE))



(g <- geo_shape(NLD_muni) +
 	geo_choropleth(col=c("pop", "gender"), convert2density=TRUE, style="kmeans")+
 	geo_borders(col="gray", lwd=1) +
 	#geo_text(NLD_muni, "code", cex=.3) +
 	geo_shape(NLD_prov, projection="robin") +
 	geo_borders(lwd=2) +
 	geo_text("name", cex=.5) +
 	geo_grid(free.scales=TRUE) +
 	geo_theme(title=c("Population", "Gender")))



