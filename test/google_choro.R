

library(plotGoogleMaps)
# Data preparation
# Point data
data(meuse)
coordinates(meuse)<-~x+y # convert to SPDF
proj4string(meuse) <- CRS("+init=epsg:28992")
# adding Coordinate Referent Sys.
# Create web map of Point data
m<-plotGoogleMaps(meuse,filename="myMap1.htm")


data(meuse)
coordinates(meuse)<-~x+y
proj4string(meuse) <- CRS('+init=epsg:28992')

m<-bubbleGoogleMaps(meuse,zcol='zinc',filename='myMap.htm')

m<-bubbleGoogleMaps(meuse,zcol='cadmium',filename='myMapCadmium.htm',layerName='Bubble plot - meuse',colPalette=terrain.colors(5),strokeColor='')
# see results in your working directory



nc <- readShapeSpatial( system.file("shapes/sids.shp",
									package="maptools")[1],
						proj4string=CRS("+proj=longlat +datum=NAD27"))
library(RColorBrewer)
m<-plotGoogleMaps(nc,zcol="NWBIR74",filename='MyMap6.htm',
				  mapTypeId='TERRAIN',colPalette= brewer.pal(7,"Reds"),
				  strokeColor="white")


data(World)
data(NLD_muni)
m <- plotGoogleMaps(World, zcol="income_grp", colPalette=brewer.pal(9, "Greens"))

m <- plotGoogleMaps(world110, zcol="income_grp", colPalette=brewer.pal(9, "Greens"))
m <- plotGoogleMaps(world3, zcol="income_grp", colPalette=brewer.pal(9, "Greens"))

world110

## check antarctica

## with shiny: http://stackoverflow.com/questions/18109815/plotgooglemaps-in-shiny-app
