## install latest version from github:
library(devtools)
install_github("geoNL", username="mtennekes", subdir="pkg")
library(geoNL)

## or load development source code (pull source code from github)
#require(devtools)
#load_all("../geoNL/pkg/")

## read data
d <- read.csv("Mminute_480.csv")

## get province shape
shp <- getShape("pv_2012.shp")

## convert coordinates to lat-long
shp <- rd2wgs84(shp)

## aggregate to NL shape
shpNL <- unionSpatialPolygons(shp, IDs=rep(1,12))

## create lat-long points object, and set constant variable x (for bubble sizes)
pnts <- SpatialPointsDataFrame(d[,1:2], data=d[,3,drop=FALSE])
pnts$x <- 1

## brewer palettes
display.brewer.all()

## show legend in order to find breaks
## for bubbleMaps, use diverging palettes only! Use a minus sign to reverse.
bubbleMap(pnts, x="x", col="cars", palette="-RdYlGn", style="kmeans", n=10, show.legend.sizes=TRUE, show.legend.colors=TRUE, scale=.2, plot.bubble.borders=FALSE, shp2=shpNL, lwd2=1, shp2.col=pal[1], title="mijn titel")

## create png's (use fixed breaks to keep colors consistent over time)
png(width=2000, height=2000, res=300)
    bubbleMap(pnts, x="x", col="cars", palette="-RdYlGn", style="fixed", breaks=c(0, 10, 22, 33, 45, 58, 74, 94, 123, 181, 400), show.legend.sizes=FALSE, show.legend.colors=FALSE, scale=.2, plot.bubble.borders=FALSE, shp2=shpNL, lwd2=1, shp2.col=pal[1], title="mijn titel")
dev.off()

## see source code of animateMaps for how to make moving gifs from png's
