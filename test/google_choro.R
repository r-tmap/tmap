

library(plotGoogleMaps)


data(World)
data(Europe)
data(NLD_muni)
library(RColorBrewer)
m <- plotGoogleMaps(World, zcol="income_grp", colPalette=brewer.pal(9, "Greens"))

m <- plotGoogleMaps(Europe, zcol="income_grp", colPalette=brewer.pal(9, "Greens"))




## with shiny: http://stackoverflow.com/questions/18109815/plotgooglemaps-in-shiny-app





# Data preparation
# Point data
data(meuse)
coordinates(meuse)<-~x+y
proj4string(meuse) <- CRS('+init=epsg:28992')
# Line data
data(meuse.grid)
coordinates(meuse.grid)<-c('x','y')
meuse.grid<-as(meuse.grid,'SpatialPixelsDataFrame')
im<-as.image.SpatialGridDataFrame(meuse.grid['dist'])
cl<-ContourLines2SLDF(contourLines(im))
proj4string(cl) <- CRS('+init=epsg:28992')

# Create web map of Point data
m<-plotGoogleMaps(meuse,filename='myMap.htm') # open the myMap, see in working directory

# Combine point and line data
mapMeusePoints<- plotGoogleMaps(meuse,add=TRUE,mapTypeId='TERRAIN')
mapMeuseCl<- plotGoogleMaps(cl,previousMap=mapMeusePoints,filename='myMap2.htm')
# see results in your working directory
