x <- read.table("../test/NDW_example/latlons.txt", sep=",", dec=".")

str(x)

library(sp)


shpNDW <- SpatialPointsDataFrame(coords=x[,3:2], data=x[,1,drop=FALSE])
shpNDW@proj4string <- CRS("+proj=longlat")

geo(shp)
plot(shpNDW)


shp <- get_shape("../test/NDW_example/cr_2013.shp")

shp <- set_projection(shp, "rd", transform=FALSE)
shp <- set_projection(shp, "longlat")

geo_shape(shp) +
	geo_borders() +
geo_shape(shpNDW) +
	geo_bubbles(size=1, col="red")