# run demo script

options(shp_dir="./inst/shapes/")

shp.gm <- getShape("gm", 2012)
shp.pv <- getShape("pv", 2012)


## load CBS-Statline data: NL population at 2012-01-01 at manucipality level
data(NLpop)

## add derived variables
NLpop <- transform(NLpop, perc.male=total.male/total*100, perc.female=total.female/total*100)


NLpop2 <- NLpop[rep(1:415, 2),]

## append data to shape file
shp.gm <- appendData(shp.gm, data=NLpop2, key.data="gm.name",key.shp="GM_NAAM")



## add population densities
shp.gm$pop.dens <- densities(shp.gm, "total")



## map gender ratio to color
shp.gm$gender <- ((shp.gm$perc.male/100)-.5) / sqrt(shp.gm$total) * 10000




###################

## plot population density map on gm level
choropleth(shp.gm, "pop.dens", style="kmeans")

## draw province borders
choropleth(shp.gm, "pop.dens", style="kmeans", shp2=shp.pv, lwd2=1)

## draw absolute population totals
bubbleMap(shp.gm, x="total")

###################

bubbleMap(shp.gm, x="total", col="gender", palette="RdBu", n=5, style="kmeans", legend.labels=c("more women", "slightly more women", "equal", "slightly more men", "more men"),
		  show.legend.sizes=TRUE, shp2=shp.pv, lwd2=2)

igeoNL()



animateMaps(choropleth(shp.gm, x=names(shp.gm)[11:16],mfrow=1, mfcol=1), filename="ani.mpg")



######################

shp.gm$test <- rgamma(415,shape=.10, scale=1)-1.5
choropleth(shp.gm, "test", style="kmeans")


## test kml
shp2kml(shp.gm, file="test.kml")


choropleth.kml(shp.gm, c("male.0_20", "male.20_65", "male.65_20"), style="kmeans")

#################
shp <- rd2wgs84(shp.gm)

ge <- GE_SpatialGrid(shp, maxPixels=1000)

png(file="test123.png", width=ge$width, height=ge$height,
	bg="transparent")
par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
choropleth(shp, x="total", style="kmeans", show.legend.text=FALSE, type.legend.plot="none", plot.bg=NA)
#plot(shp, col="blue")
dev.off()

x <- kmlOverlay(ge, "test123.kml", "test123.png")








