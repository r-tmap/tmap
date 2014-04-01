set_bounding_box <- function(gp, gt) {
	lapply(gp, function(gpl) {
		shp <- gpl$shp
		
		bb <- shp@bbox
		
		if (gt$draw.frame) {
			bbcoords <- cbind(x=bb[1,][c(1, 1, 2, 2, 1)], y=bb[2,][c(1, 2, 2, 1, 1)])
			
			BB <- SpatialPolygons(list(Polygons(list(Polygon(bbcoords)), "1")),
								  proj4string=CRS(proj4string(shp)))
			
			shp2 <- gIntersection(shp, BB, byid=TRUE)
			shp2@bbox <- bb
			shpdata <- shp@data
			ids <- get_IDs(shp)
			ids2 <- gsub(" [0-9]+$", "", get_IDs(shp2))
			indices <- match(ids2, ids)
			shp2 <- SpatialPolygonsDataFrame(shp2, shpdata[indices, ], match.ID = FALSE)
			shp <- shp2
			
			if (length(gpl$fill)==length(ids)) gpl$fill <- gpl$fill[indices]
			if (length(gpl$bubble.size)==length(ids)) gpl$bubble.size <- gpl$bubble.size[indices]
			
		}
		gpl$shp <- shp
		gpl
	})
}