get_sf_coordinates <- function(shp, gpl, gt, bbx) {
	
	id <- NULL
	
	if (any(st_geometry_type(shp) %in% c("MULTILINESTRING", "MULTIPOINT"))) {
		if (gt$shape.point.per=="segment") {
			ns <- nrow(shp)
			shp <- sf_expand(shp)
			id <- shp$split__id
		} else if (gt$shape.point.per == "largest") {
			if (st_geometry_type(shp)[1] %in% c("LINESTRING", "MULTILINESTRING")) {
				ns <- nrow(shp)
				shp2 <- sf_expand(shp)
				lengths <- st_length(shp2)
				id_max <- sapply(split(lengths, f=shp2$split__id), which.max)
				id_max2 <- sapply(1L:ns, function(i) {
					which(shp2$split__id==i)[id_max[i]]
				})
				shp <- shp2[id_max2,]
			} else if (st_geometry_type(shp)[1] %in% c("POINT", "MULTIPOINT")) {
				# take the first one
				
				ns <- nrow(shp)
				shp2 <- sf_expand(shp)
				lengths <- st_length(shp2)
				id_max2 <- sapply(1L:ns, function(i) {
					which(shp2$split__id==i)[1]
				})
				shp <- shp2[id_max2,]
			} # Polygons: see of_largest_polygon
		}
	}
	
	
	
	if (st_geometry_type(shp)[1] %in% c("LINESTRING", "MULTILINESTRING")) {
		if (gt$shape.line.center=="midpoint") {
			coor <- st_coordinates(shp)
			coors <- split.data.frame(coor[,1:2], f = coor[,ncol(coor)], drop=FALSE)
			co <- do.call(rbind, lapply(coors, get_midpoint))
		} else {
			co <- suppressWarnings(st_coordinates(st_centroid(shp)))
		}
	} else if (st_geometry_type(shp)[1] %in% c("POLYGON", "MULTIPOLYGON"))  {
		co <- suppressWarnings(st_coordinates(st_centroid(shp, of_largest_polygon = (gt$shape.point.per == "largest"))))
	} else {
		co <- suppressWarnings(st_coordinates(shp))
	}

	if (!is.null(id)) {
		aes <- intersect(names(gpl), c("fill.col", "line.col", "line.lwd", "text", "text.size", "text.color", "text.xmod", "text.ymod", "text_sel", "symbol.size", "symbol.col", "symbol.shape", "symbol.xmod", "symbol.ymod"))
		gpl[aes] <- lapply(gpl[aes], function(a) {
			if (length(a)==ns) {
				a[id]
			} else a
		})
	}
	
	
	list(co=co, gpl=gpl, shp=shp)
}
