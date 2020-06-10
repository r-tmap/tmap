grid.shape <- function(shp, gp=gpar(), bg.col=NA, i, k) {
	geoms <- st_geometry(shp)
	bb <- attr(shp, "bbox")
	
	# do.call("gList", mapply(function(p, id1) {
	# 	gp2 <- lapply(gp, function(g) {
	# 		if (length(g)==nrow(shp)) g[id1] else g
	# 	})
	# 	class(gp2) <- "gpar"
	# 
	# 	grb <- st_as_grob(p, gp=gp2, name=paste("tm_polygons", i, k, id1, sep="_"))
	# 	#grob_mod(grb, x.a=x.a, x.b=x.b, y.a=y.a, y.b=y.b)
	# 	grb
	# 
	# }, geoms, 1:nrow(shp), SIMPLIFY=FALSE))
	st_as_grob(geoms, gp = gp,name = paste("tm_polygons", i, k, sep = "_"))
}



grid.shplines <- function(shp, gp=gpar(), i, k) {
	geoms <- st_geometry(shp)
	bb <- attr(shp, "bbox")
	
	geoms <- st_cast(geoms, "MULTILINESTRING")
	
	# do.call("gList", mapply(function(p, id1) {
	# 	gp2 <- lapply(gp, function(g) {
	# 		if (length(g)==nrow(shp)) g[id1] else g
	# 	})
	# 	class(gp2) <- "gpar"
	# 	
	# 	grb <- st_as_grob(p, gp=gp2, name=paste("tm_lines", i, k, id1, sep="_"))
	# 	#grob_mod(grb, x.a=x.a, x.b=x.b, y.a=y.a, y.b=y.b)
	# 	grb
	# 	
	# }, geoms, 1:nrow(shp), SIMPLIFY=FALSE))
	st_as_grob(geoms, gp = gp,name = paste("tm_lines", i, k, sep = "_"))
	
}
