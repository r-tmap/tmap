# run demo script

options(shp_dir="./inst/shapes/")

shp.gm <- getShape("gm", 2012)
shp.pv <- getShape("pv", 2012)


## load CBS-Statline data: NL population at 2012-01-01 at manucipality level
data(NLpop)

## add derived variables
NLpop <- transform(NLpop, perc.male=total.male/total*100, perc.female=total.female/total*100)

## append data to shape file
shp.gm <- appendData(shp.gm, data=NLpop, key.data="gm.name",key.shp="GM_NAAM")

## add population densities
shp.gm$pop.dens <- densities(shp.gm, "total")

## map gender ratio to color
shp.gm$gender <- ((shp.gm$perc.male/100)-.5) / sqrt(shp.gm$total) * 10000



(g <- geo.choropleth(shp.gm, col=c("total", "gender"), style="kmeans") +
	geo.borders(shp.gm, col="gray", lwd=1) +
	geo.borders(shp.pv, lwd=2) +
	geo.text(shp.pv, c("PV_NAAM", "STATCODE"), cex=.5) +
 	geo.grid(free.scales=FALSE))




(g <- geo.bubblemap(shp.gm, col=c("total", "GM2012"), style="kmeans") +
 	geo.borders(shp.gm, col="gray", lwd=1) +
 	geo.borders(shp.pv, lwd=2) +
 	geo.text(shp.pv, "PV_NAAM", cex=.5) +
 	geo.grid(free.scales=TRUE))



(g <- geo.choropleth(shp.gm, col=c("total", "gender"), style="kmeans") +
 	geo.borders(shp.gm, col="gray", lwd=1) +
 	geo.borders(shp.pv, lwd=2) +
	geo.bubblemap(shp.gm, size=c("total", "GM2012"), style="kmeans") +
 	geo.text(shp.pv, "PV_NAAM", cex=.5) +
 	geo.grid(free.scales=TRUE) +
 	geo.zoom(xlim=c(.4, .6), ylim=c(.4,.6)))


(g <- geo.choropleth(shp.gm, col=c("total", "gender"), style="kmeans") +
 	geo.borders(shp.gm, col="gray", lwd=1) +
 	geo.borders(shp.pv, lwd=2) +
 	geo.text(shp.pv, "PV_NAAM", cex=.5) +
 	geo.bubblemap(shp.gm, size=c("total", "GM2012"), col=gray(seq(0,1,length.out=20)), style="kmeans", border="darkblue") +
 	geo.grid(free.scales=TRUE) + 
 	geo.theme(title="bubble.size", legend.only=TRUE))



(g <- geo.choropleth(shp.gm, col="total", style="kmeans") +
 	geo.borders(shp.gm, col="gray", lwd=1) +
	geo.borders(shp.pv, lwd=2) +
	geo.text(shp.pv, "PV_NAAM", cex=.5))

(g <- geo.choropleth(shp.gm, col=c("total", "gender"), style="kmeans") +
 	geo.borders(shp.gm, col="gray", lwd=1) +
 	geo.borders(shp.pv, lwd=2) +
 	geo.text(shp.pv, "PV_NAAM", cex=.5) +
 	geo.bubblemap(shp.gm, size=c("total", "GM2012"), col=gray(seq(0,1,length.out=20)), style="kmeans", border="darkblue") +
 	geo.grid(free.scales=TRUE) + 
 	geo.theme(title="bubble.size"))

(g <- geo.choropleth(shp.gm, col=c("total", "gender"), style="kmeans") +
 	geo.borders(shp.gm, col="gray", lwd=1) +
 	geo.borders(shp.pv, lwd=2) +
 	geo.text(shp.pv, "PV_NAAM", cex=.5) +
 	geo.bubbles(shp.gm, size=1, col=gray(seq(0,1,length.out=20)), border="darkblue") +
 	geo.grid(free.scales=TRUE) + 
 	geo.theme(title="bbble.size"))



library(rworldmap)
data(countryExData)
sPDF <- joinCountryData2Map( countryExData, joinCode = "ISO3", nameJoinColumn = "ISO3V10")

str(sPDF)

mapCountryData( sPDF, nameColumnToPlot="BIODIVERSITY" )
names(sPDF)

sPDF$BIODIVERSITY[is.na(sPDF$BIODIVERSITY)] <- 0
geo.choropleth(sPDF, col="BIODIVERSITY") + geo.borders(sPDF) + geo.text(sPDF, text="ISO_A3", cex=.4)


