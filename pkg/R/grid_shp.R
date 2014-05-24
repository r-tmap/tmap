grid.shape <- function(shp, gp=gpar()) {
	# TODO substract holes
	bb <- bbox(shp)
	co1 <- do.call("rbind", lapply(shp@polygons, function(p) {
		id1 <- substitute(p)[[3]]
		co2 <- lapply(p@Polygons, function(pp) {
			id2 <- substitute(pp)[[3]]
			coords <- pp@coords
			coords[,1] <- (coords[,1]-bb[1,1]) / (bb[1,2]-bb[1,1])
			coords[,2] <- (coords[,2]-bb[2,1]) / (bb[2,2]-bb[2,1])
			cbind(coords, id2)
		})
		cbind(do.call("rbind", co2), id1)
	}))
	id <- as.integer(as.factor(co1[,4]*10000+co1[,3]))
	id2 <- co1[,4][!duplicated(id)]
	gp2 <- lapply(gp, function(g) {
		if (length(g)==length(shp)) g[id2] else g
	})
	class(gp2) <- "gpar"
	grid.polygon(co1[,1], co1[,2],	id=id, gp=gp2)
	invisible()
}

grid.shplines <- function(shp, gp=gpar()) {
	bb <- bbox(shp)
	co1 <- do.call("rbind", lapply(shp@lines, function(p) {
		id1 <- substitute(p)[[3]]
		co2 <- lapply(p@Polygons, function(pp) {
			id2 <- substitute(pp)[[3]]
			coords <- pp@coords
			coords[,1] <- (coords[,1]-bb[1,1]) / (bb[1,2]-bb[1,1])
			coords[,2] <- (coords[,2]-bb[2,1]) / (bb[2,2]-bb[2,1])
			cbind(coords, id2)
		})
		cbind(do.call("rbind", co2), id1)
	}))
	id <- as.integer(as.factor(co1[,4]*10000+co1[,3]))
	id2 <- co1[,4][!duplicated(id)]
	gp2 <- lapply(gp, function(g) {
		if (length(g)==length(shp)) g[id2] else g
	})
	class(gp2) <- "gpar"
	grid.polyline(co1[,1], co1[,2],	id=id, gp=gp2)
	invisible()
}
