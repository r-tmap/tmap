data(europe)
data(world)
data(NLD_muni)
data(NLD_prov)
data(NLD_ageGroups)


## add derived variables
NLpop <- transform(NLpop, perc.male=total.male/total*100, perc.female=total.female/total*100)

## append data to shape file
shp.gm <- appendData(shp.gm, data=NLpop, key.data="gm.name",key.shp="GM_NAAM")

## add population densities
shp.gm$pop.dens <- densities(shp.gm, "total")

## map gender ratio to color
shp.gm$gender <- ((shp.gm$perc.male/100)-.5) / sqrt(shp.gm$total) * 10000


names(NLD_muni)
geo_choropleth(NLD_muni, "pop", convert2density=TRUE, style="kmeans")


shp_Wrld <- world
shp_Europe <- europe
shp_NLD_prov <- NLD_prov
shp_NLD_muni <- NLD_muni

NLD_muni$gender <- (NLD_muni$men/NLD_muni$pop -.5) / sqrt(NLD_muni$pop) * 10000

# to do: zooming: borders and text

(g <- geo_shape(NLD_muni) +
 	geo_choropleth(NLD_muni, col=c("pop", "gender"), convert2density=TRUE, style="kmeans")+
 	geo_borders(NLD_muni, col="gray", lwd=1) +
 	#geo_text(NLD_muni, "code", cex=.3) +
 	geo_shape(NLD_prov) +
 	geo_borders(NLD_prov, lwd=2) +
 	#geo_text(NLD_prov, "name", cex=.5) +
 	geo_grid(free.scales=TRUE) +
 	geo_theme(title=c("Population", "Gender")) + 
 	geo_zoom(c(.3, .8), c(.3, .8), units="rel"))



(g <- geo_choropleth(NLD_muni, col=c("pop", "gender"), convert2density=TRUE, style="kmeans")+
 	geo_borders(NLD_muni, col="gray", lwd=1) +
 	geo_text(NLD_muni, "code", cex=.3) +
 	geo_borders(NLD_prov, lwd=2) +
 	geo_text(NLD_prov, "name", cex=.5) +
 	geo_grid(free.scales=TRUE) +
 	geo_theme(title=c("Population", "Gender")) + 
 	geo_zoom(c(.3, .5), c(.3, .5), units="rel"))

