data(europe)
data(world)
data(NLD_muni)
data(NLD_prov)
data(NLD_ageGroups)

NLD_muni$gender <- (NLD_muni$men/NLD_muni$pop -.5) / sqrt(NLD_muni$pop) * 10000



names(NLD_muni)
geo_shape(NLD_muni) +
geo_choropleth(col="pop", convert2density=TRUE, style="kmeans") +
geo_borders() +
geo_bubblemap(size="men") +
geo_frame(c(.3, .8), c(.3, .8), units="rel")


# to do: zooming: borders and text

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

