library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)

## read world shape files from http://www.naturalearthdata.com/features/

wrld <- getShape("../shapes/ne_50m_admin_0_countries_lakes.shp")
world <- getShape("../shapes/ne_110m_admin_0_countries_lakes.shp")


#Download the continents shapefile
# download.file("http://baruch.cuny.edu/geoportal/data/esri/world/continent.zip", "cont.zip") 
#Unzip it
# unzip("cont.zip")
#Load it
cont <- getShape("../shapes/continent.shp")


proj4string(wrld) <- "+proj=longlat +datum=WGS84"

wrld$continent[wrld$admin=="Turkey"] <- "Europe"

eur <- wrld[wrld$continent=="Europe", ]

plot(eur)


## global cropping
#CP <- as(extent(-25, 50, 34, 72), "SpatialPolygons")
CP <- as(extent(-25, 70, 34, 82), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(eur))

eur2 <- gIntersection(eur, CP, byid=TRUE)

plot(eur2)

## making use of tz to cut russia
conteur <- cont[cont$CONTINENT=="Europe",]
plot(conteur)
proj4string(conteur) <- "+proj=longlat +datum=WGS84"

CP <- as(extent(-32, 48, 34, 72), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(conteur))
conteur2 <- gUnion(conteur, CP, byid=TRUE)
plot(conteur2)

CP <- as(extent(40, 64, 67, 70.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(tzeur))
conteur3 <- gUnion(conteur2, CP, byid=TRUE)
geo.borders(conteur3)

CP <- as(extent(10, 75, 72, 85), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(tzeur))
conteur4 <- gDifference(conteur3, CP, byid=TRUE)
plot(conteur4)

CP <- as(extent(48, 75, 70.5, 85), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(tzeur))
conteur5 <- gDifference(conteur4, CP, byid=TRUE)
plot(conteur5)

eur3 <- gIntersection(eur2, conteur5, byid=TRUE)

plot(eur3)

eur4 <- spTransform(eur3 ,CRS("+proj=utm +zone=33 +north"))

geo.borders(eur4) + 
	geo.theme(draw.frame=TRUE) +
	geo.fill(eur4)



plot(world)
