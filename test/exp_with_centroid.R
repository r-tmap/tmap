shp <- Europe


	
maxID <- sapply(slot(shp, "polygons"),
			function(x) which.max(sapply(slot(x, "Polygons"), slot, "area")))

shp@polygons <- mapply(function(x, id) {
	x@Polygons <- x@Polygons[id]
	x@plotOrder <- 1L
	x@area <- x@Polygons[[1]]@area
	x
}, shp@polygons, maxID, SIMPLIFY=FALSE)
slot(shp, "polygons") <- lapply(slot(shp, "polygons"),
								   "comment<-", NULL) 


library(rgeos)
slot(shp, "polygons") <- lapply(slot(shp, "polygons"),
								 checkPolygonsHoles) 



gIsValid(shp, reason = TRUE)



trueCentroids = gCentroid(shp,byid=TRUE)
plot(Europe)
points(coordinates(Europe),pch=1)
points(trueCentroids,pch=2)
