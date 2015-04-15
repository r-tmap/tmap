data(NLD_prov)
data(NLD_muni)

## tmap
system.time({
	print(tm_shape(NLD_muni) +
		tm_fill("population", palette = "Reds", convert2density = TRUE, style="kmeans") +
		tm_borders() +
	tm_shape(NLD_prov) +
		tm_borders(lwd = 2) +
	tm_layout("Population (per km2)", inner.margins=c(.02,.2, .06, .02)))
})

## sp
library(sp)
library(rgeos)
library(RColorBrewer)
library(classInt)

system.time({

	areas <- gArea(NLD_muni, byid=TRUE) 
	pop_dens <- NLD_muni$population / areas * 1e6
	
	cls <- classIntervals(pop_dens, n=5, style = "kmeans")#, pal=brewer.pal(9, "Reds"))
	nbrks <- length(cls$brks)
	
	pal <- brewer.pal(nbrks-1, "Reds")
	
	cols <- pal[findCols(cls)]
	
	p <- par(mar=c(1, 1, 1, 1))
	plot(NLD_muni, col=cols, border="grey40")
	plot(NLD_prov, border="grey40", lwd = 2, add=TRUE)
	#title("Population (per km2)")
	
	brks <- round(cls$brks)
	
	legend("topleft", legend=paste(brks[-nbrks], brks[-1], sep=" to "), fill=pal, bty="n", y.intersp=0.8, title="Population (per km2)")

})


## ggplot2
library(ggplot2)

system.time({
	x <- fortify(NLD_muni, region = "code")
	
	NLD_muni$area <- gArea(NLD_muni, byid=TRUE) 
	NLD_muni$pop_dens <- NLD_muni$population / NLD_muni$area * 1e6
	NLD_muni$pop_dens_class <- cut(NLD_muni$pop_dens, breaks=classIntervals(NLD_muni$pop_dens, n = 5, style = 'kmeans')$brks)
	
	x <- merge(x, NLD_muni@data, by.x = "id", by.y = "code")
	
	y <- fortify(NLD_prov, region = "code")
	
	print(ggplot(x, aes(x=long, y=lat, group=group, fill=pop_dens_class)) + geom_polygon(color="grey40") + scale_fill_brewer("Population (per km2)", palette="Reds") + geom_polygon(aes(x=long, y=lat, group=group, fill=NA), data=y, color="grey40", size=1))

})

## choroplethr
library(choroplethr)
library(choroplethrMaps)
# how??


## rworldmap
library(rworldmap)

colours=brewer.pal(5,"OrRd")

NLD_muni$area <- gArea(NLD_muni, byid=TRUE) 
NLD_muni$pop_dens <- NLD_muni$population / NLD_muni$area * 1e6

mapParams <- mapPolys( NLD_muni
					   ,nameColumnToPlot='pop_dens'
					   ,catMethod="quantiles"
					   ,numCats=5
					   ,colourPalette=colours
					   ,addLegend=FALSE )


do.call( addMapLegend, c( mapParams
						  , legendLabels="all"
						  , legendWidth=0.5
))

