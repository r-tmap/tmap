set_bounding_box <- function(shps, gp, gt) {
	dw <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)
	dh <- convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE)
	dasp <- dw/dh
	pasp <- gt$asp
	
	if (identical(pasp, 0)) pasp <- dasp
	
	mapply(function(shp, gpl) {
		bb <- shp@bbox
		bbrange <- bb[,2] - bb[,1]
		bbmarg <- gt$inner.margins[c(2,1,4,3)]
		bbmarg[c(1,2)] <- -bbmarg[c(1,2)]
		bb <- bb + rep(bbrange, 2) * bbmarg
		
		if (!is.na(pasp)) {
			xlim <- bb[1,]
			ylim <- bb[2,]
			
			longlat <- !is.projected(shp)
			
			sasp <- if(longlat) {
				(diff(xlim)/diff(ylim)) * cos((mean(ylim) * pi)/180)
			} else {
				(diff(xlim)/diff(ylim))# * 2
			}
			
			if (pasp > sasp) {
				## landscape device
				xdiff <- if (longlat) diff(ylim) * pasp / cos((mean(ylim) * pi)/180) else diff(ylim) * (pasp)
				bb[1, ] <- mean(xlim) + (xdiff * c(-.5, .5))
			} else {
				## portrait device
				ydiff <- if (longlat) (diff(xlim) * cos((mean(ylim) * pi)/180)) / pasp else diff(xlim) / (pasp)
				bb[2, ] <- mean(ylim) + (ydiff * c(-.5, .5))
			}
		}
		
		
		## try to crop the shape file at the bounding box in order to place bubbles and text labels inside the frame
		bbcoords <- cbind(x=bb[1,][c(1, 1, 2, 2, 1)], y=bb[2,][c(1, 2, 2, 1, 1)])
		shp2 <- tryCatch({
			BB <- SpatialPolygons(list(Polygons(list(Polygon(bbcoords)), "1")),
								  proj4string=CRS(proj4string(shp)))
			gIntersection(shp, BB, byid=TRUE)
		}, error = function(e) {
			shp
		})
		shp2@bbox <- bb
		shpdata <- shp@data
		ids <- get_IDs(shp)
		ids2 <- gsub(" [0-9]+$", "", get_IDs(shp2))
		indices <- match(ids2, ids)
		shp2 <- SpatialPolygonsDataFrame(shp2, shpdata[indices, ], match.ID = FALSE)
		shp <- shp2
		
		if (length(gpl$fill)==length(ids)) gpl$fill <- gpl$fill[indices]
		if (length(gpl$bubble.size)==length(ids)) gpl$bubble.size <- gpl$bubble.size[indices]
		if (length(gpl$bubble.col)==length(ids)) gpl$bubble.col <- gpl$bubble.col[indices]
		
		list(shp=shp2, layer=gpl)
	}, shps, gp, SIMPLIFY=FALSE)
}