data(Europe)
data(World)
data(NLD_muni)
data(NLD_prov)
data(NLD_ageGroups)

NLD_muni$gender <- (NLD_muni$men/NLD_muni$pop -.5) / sqrt(NLD_muni$pop) * 10000

World$pop_est_km <- densities(World, "pop_est", total.area.km2=148940000)

head(World[order(World$pop_est_km, decreasing=TRUE), c("name", "pop_est", "pop_est_km")], 100)
areas <- getAreas(World)
World$area <- areas * (148940000 / sum(areas))
head(World[order(World$area, decreasing=TRUE), c("name", "area")], 100)


names(NLD_muni)
geo_shape(NLD_muni) +
geo_choropleth(col="pop", convert2density=TRUE, style="kmeans") +
geo_borders() +
geo_bubblemap(size="men") +
geo_frame(c(.3, .8), c(.3, .8), units="rel")


(g <- geo_shape(World) +
 	geo_choropleth("income_grp") +
 	geo_borders() +
 	geo_theme(legend.position=c("left", "bottom"), legend.plot.size=c(.2, .2), legend.cex=0.6, draw.frame=TRUE))

#log scale from 0 to 30000
log1p(5000)
exp(seq(0, 8.5, length.out=7))

(g <- geo_shape(World) +
 	geo_choropleth("pop_est_km", style="fixed", breaks=c(0, 5, 20, 100, 250, 1000, 30000), palette="YlOrRd", auto.palette.mapping=FALSE) +
 	geo_borders() +
 	geo_theme(legend.position=c("left", "bottom"), type.legend.plot="none", legend.plot.size=c(.2, .2), legend.cex=0.6, draw.frame=TRUE))



(g <- geo_shape(World) +
 	geo_choropleth("income_grp") +
 	geo_borders() +
 	geo_bubblemap("pop_est") +
 	geo_theme(legend.position=c("left", "bottom"), legend.plot.size=c(.2, .2), legend.cex=0.6, draw.frame=FALSE))

(g <- geo_shape(Europe) +
	geo_choropleth("gdp_cap_est", style="kmeans") +
 	geo_borders() +
 	geo_bubblemap("pop_est", scale=5) +
 	geo_theme(legend.position=c("left", "top"), legend.plot.size=c(.3, .25), legend.cex=0.6, draw.frame=FALSE))


(g <- geo_shape(NLD_prov) +
 	geo_choropleth("pop", style="kmeans", convert2density=TRUE) + geo_theme(draw.frame=TRUE))



(g <- geo_shape(NLD_muni) +
 	geo_choropleth(col=c("pop", "gender"), convert2density=TRUE, style="kmeans")+
 	geo_borders(col="gray", lwd=1) +
 	#geo_text(NLD_muni, "code", cex=.3) +
 	geo_shape(NLD_prov) +
 	geo_borders(lwd=2) +
 	geo_text("name", cex=.5) +
 	geo_grid(free.scales=TRUE) +
 	geo_theme(title=c("Population", "Gender")) + 
 	geo_zoom(c(.3, .8), c(.3, .8), units="rel"))



(g <- geo_shape(NLD_muni) +
 	geo_choropleth(col=c("pop", "gender"), convert2density=TRUE, style="kmeans")+
 	geo_borders(col="gray", lwd=1) +
 	geo_text("code", cex=.3) +
 	geo_shape(NLD_prov) +
 	geo_borders(lwd=2) +
 	geo_text("name", cex=.5) +
 	geo_grid(free.scales=TRUE) +
 	geo_theme(title=c("Population", "Gender")) + 
 	geo_zoom(c(.3, .5), c(.3, .5), units="rel"))

(g <- geo_shape(World) +
 	geo_borders() +
 	geo_theme(draw.frame=TRUE))