## current version: all polygons in one group
grid.shape_old <- function(shp, gp=gpar(), bg.col=NA) {
	# TODO substract holes
	bb <- bbox(shp)
	co1 <- do.call("rbind", mapply(function(p, id1) {
		co2 <- mapply(function(pp, id2) {
			coords <- pp@coords
			coords[,1] <- (coords[,1]-bb[1,1]) / (bb[1,2]-bb[1,1])
			coords[,2] <- (coords[,2]-bb[2,1]) / (bb[2,2]-bb[2,1])
			cbind(coords, id2, pp@hole)
		}, p@Polygons, 1:length(p@Polygons), SIMPLIFY=FALSE)
		cbind(do.call("rbind", co2), id1)
	}, shp@polygons, 1:length(shp), SIMPLIFY=FALSE))
	id <- as.integer(as.factor(co1[,5]*10000+co1[,3]))
	id2 <- co1[,5][!duplicated(id)]
	holes <- as.logical(co1[,4][!duplicated(id)])
	gp2 <- lapply(gp, function(g) {
		if (length(g)==length(shp)) g[id2] else g
	})
	if ("fill" %in% names(gp2)) {
		if (length(gp2$fill)==length(holes)) {
			gp2$fill[holes] <- bg.col 
		} else {
			bg.col <- ifelse(is.na(gp2$fill), NA, bg.col)
			gp2$fill <- ifelse(holes, bg.col, gp2$fill[1])
		}
	}
	
	class(gp2) <- "gpar"
	idNames <- paste("tm_polygons", id, sep="_")
	polygonGrob(co1[,1], co1[,2],	id=id, gp=gp2, name = idNames)
}


## polygons grouped by ID (version 2)
## slower than version above:

# data(World)
# gp <- list(fill=rainbow(177), lwd=2)
# shp <- World
# microbenchmark(
# 	x <- grid.shape(shp, gp),
# 	x2 <- grid.shape_old(shp, gp))
# 
# microbenchmark(
# 	x2 <- grid.shape_old(shp, gp),
# 	print(tm_shape(World)+ tm_polygons(col = "pop_est_dens")), times=10L)

grid.shape <- function(shp, gp=gpar(), bg.col=NA) {
	bb <- bbox(shp)
	co1 <- do.call("gList", mapply(function(p, id1) {
		np <- length(p@Polygons)
		co2 <- mapply(function(pp, id2) {
			coords <- pp@coords
			cbind(coords, id2)
		}, p@Polygons, 1:length(p@Polygons), SIMPLIFY=FALSE)
		res <- cbind(do.call("rbind", co2))
		res[,1] <- (res[,1]-bb[1,1]) / (bb[1,2]-bb[1,1])
		res[,2] <- (res[,2]-bb[2,1]) / (bb[2,2]-bb[2,1])
		
		
		holes <- sapply(p@Polygons, "attr", "hole")
		gp2 <- lapply(gp, function(g) {
			if (length(g)==length(shp)) rep.int(g[id1], np) else g
		})
		if ("fill" %in% names(gp2)) {
			if (length(gp2$fill)==np) {
				gp2$fill[holes] <- bg.col 
			} else {
				bg.col <- ifelse(is.na(gp2$fill), NA, bg.col)
				gp2$fill <- ifelse(holes, bg.col, gp2$fill[1])
			}
		}		
		class(gp2) <- "gpar"
		idName <- paste("tm_polygons", id1, sep="_")
		polygonGrob(res[,1], res[,2], id=res[,3], gp=gp2, name = idName)
		
	}, shp@polygons, 1:length(shp), SIMPLIFY=FALSE))
}




grid.shplines <- function(shp, gp=gpar()) {
	bb <- bbox(shp)
	co1 <- do.call("rbind", mapply(function(p, id1) {
		co2 <- mapply(function(pp, id2) {
			coords <- pp@coords
			coords[,1] <- (coords[,1]-bb[1,1]) / (bb[1,2]-bb[1,1])
			coords[,2] <- (coords[,2]-bb[2,1]) / (bb[2,2]-bb[2,1])
			cbind(coords, id2)
		}, p@Lines, 1:length(p@Lines), SIMPLIFY=FALSE)
		cbind(do.call("rbind", co2), id1)
	}, shp@lines, 1:length(shp@lines), SIMPLIFY=FALSE))
	id <- as.integer(as.factor(co1[,4]*10000+co1[,3]))
	id2 <- co1[,4][!duplicated(id)]
	gp2 <- lapply(gp, function(g) {
		if (length(g)==length(shp)) g[id2] else g
	})
	class(gp2) <- "gpar"
	polylineGrob(co1[,1], co1[,2],	id=id, gp=gp2)
}
