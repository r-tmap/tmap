grid.shape <- function(shp, gp=gpar(), bg.col=NA, i, k) {
	geoms <- st_geometry(shp)
	
	
	## TO DO relative coordinates
	
	bb <- attr(shp, "bbox")
	
	# easy fix
	# x.b <- 1/ (bb[3] - bb[1])
	# x.a <- -bb[1] * x.b
	# y.b <- 1/ (bb[4] - bb[2])
	# y.a <- -bb[2] * y.b
	do.call("gList", mapply(function(p, id1) {	
		gp2 <- lapply(gp, function(g) {
			if (length(g)==nrow(shp)) g[id1] else g
		})
		class(gp2) <- "gpar"
		
		grb <- st_as_grob(p, gp=gp2, name=paste("tm_polygons", i, k, id1, sep="_"))
		#grob_mod(grb, x.a=x.a, x.b=x.b, y.a=y.a, y.b=y.b)
		grb
		
	}, geoms, 1:nrow(shp), SIMPLIFY=FALSE))
	

	
	# bb <- bbox(shp)
	# do.call("gList", mapply(function(p, id1) {
	# 	np <- length(p@Polygons)
	# 	co2 <- mapply(function(pp, id2) {
	# 		coords <- pp@coords
	# 		cbind(coords, id2)
	# 	}, p@Polygons, 1:length(p@Polygons), SIMPLIFY=FALSE)
	# 	res <- cbind(do.call("rbind", co2))
	# 	res[,1] <- (res[,1]-bb[1,1]) / (bb[1,2]-bb[1,1])
	# 	res[,2] <- (res[,2]-bb[2,1]) / (bb[2,2]-bb[2,1])
	# 	
	# 	gp2 <- lapply(gp, function(g) {
	# 		if (length(g)==length(shp)) g[id1] else g
	# 	})
	# 	class(gp2) <- "gpar"
	# 	idName <- paste("tm_polygons", i, k, id1, sep="_")
	# 	pathGrob(res[,1], res[,2], id=res[,3], gp=gp2, name = idName)
	# 	
	# }, shp@polygons, 1:length(shp), SIMPLIFY=FALSE))
}



grid.shplines <- function(shp, gp=gpar(), i, k) {
	geoms <- st_geometry(shp)
	bb <- attr(shp, "bbox")
	
	# easy fix
	# x.b <- 1/ (bb[3] - bb[1])
	# x.a <- -bb[1] * x.b
	# y.b <- 1/ (bb[4] - bb[2])
	# y.a <- -bb[2] * y.b
	
	geoms <- st_cast(geoms, "MULTILINESTRING")
	
	do.call("gList", mapply(function(p, id1) {	
		gp2 <- lapply(gp, function(g) {
			if (length(g)==nrow(shp)) g[id1] else g
		})
		class(gp2) <- "gpar"
		
		grb <- st_as_grob(p, gp=gp2, name=paste("tm_lines", i, k, id1, sep="_"))
		#grob_mod(grb, x.a=x.a, x.b=x.b, y.a=y.a, y.b=y.b)
		grb
		
	}, geoms, 1:nrow(shp), SIMPLIFY=FALSE))
	# 
	# 
	# 
	# bb <- bbox(shp)
	# do.call("gList", mapply(function(p, id1) {
	# 	np <- length(p@Lines)
	# 	co2 <- mapply(function(pp, id2) {
	# 		coords <- pp@coords
	# 		cbind(coords, id2)
	# 	}, p@Lines, 1:length(p@Lines), SIMPLIFY=FALSE)
	# 	res <- cbind(do.call("rbind", co2), id1)
	# 	res[,1] <- (res[,1]-bb[1,1]) / (bb[1,2]-bb[1,1])
	# 	res[,2] <- (res[,2]-bb[2,1]) / (bb[2,2]-bb[2,1])
	# 
	# 	gp2 <- lapply(gp, function(g) {
	# 		if (length(g)==length(shp)) rep.int(g[id1], np) else g
	# 	})
	# 	class(gp2) <- "gpar"
	# 	idName <- paste("tm_lines", i, k, id1, sep="_")
	# 	polylineGrob(res[,1], res[,2],	id=res[,3], gp=gp2, name=idName)
	# }, shp@lines, 1:length(shp), SIMPLIFY=FALSE))
}
