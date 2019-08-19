get_sf_coordinates <- function(shp, gpl) {

	point.per <- attr(shp, "point.per")
	line.center <- attr(shp, "line.center")
	

	tmapID <- shp$tmapID
	
	id <- NULL
	if (any(st_geometry_type(shp) %in% c("MULTILINESTRING", "MULTIPOINT", "MULTIPOLYGON"))) {
		if (point.per=="segment") {
			ns <- nrow(shp)
			shp <- sf_expand(shp)
			id <- shp$split__id
		} else if (point.per == "largest") {
			if (st_geometry_type(shp)[1] %in% c("LINESTRING", "MULTILINESTRING")) {
				ns <- nrow(shp)
				shp2 <- sf_expand(shp)
				lengths <- st_length(shp2)
				id_max <- vapply(split(lengths, f=shp2$split__id), which.max, integer(1))
				id_max2 <- vapply(1L:ns, function(i) {
					which(shp2$split__id==i)[id_max[i]]
				}, integer(1))
				shp <- shp2[id_max2,]
			} else if (st_geometry_type(shp)[1] %in% c("POINT", "MULTIPOINT")) {
				# take the first one
				
				ns <- nrow(shp)
				shp2 <- sf_expand(shp)
				lengths <- st_length(shp2)
				id_max2 <- vapply(1L:ns, function(i) {
					which(shp2$split__id==i)[1]
				}, integer(1))
				shp <- shp2[id_max2,]
			} # Polygons: see of_largest_polygon
		}
	}
	

	if (st_geometry_type(shp)[1] %in% c("LINESTRING", "MULTILINESTRING")) {
		if (line.center=="midpoint") {
			coor <- st_coordinates(shp)
			coors <- split.data.frame(coor[,1:2], f = coor[,ncol(coor)], drop=FALSE)
			co <- do.call(rbind, lapply(coors, get_midpoint))
		} else {
			co <- get_centroids(shp)
		}
	} else if (st_geometry_type(shp)[1] %in% c("POLYGON", "MULTIPOLYGON"))  {
		co <- get_centroids(shp, of_largest_polygon = (point.per == "largest"))
	} else {
		co <- suppressWarnings(st_coordinates(shp))
	}

	if (!is.null(id)) {
		aes <- intersect(names(gpl), c("fill", "line.col", "line.lwd", "text", "text.size", "text.color", "text.xmod", "text.ymod", "text_sel", "symbol.size", "symbol.col", "symbol.shape", "symbol.xmod", "symbol.ymod"))
		gpl[aes] <- lapply(gpl[aes], function(a) {
			if (length(a)==ns) {
				a[id]
			} else a
		})
		if ("data" %in% names(gpl)) gpl$data <- gpl$data[id, , drop = FALSE]
		shp$tmapID <- tmapID[id]
		shp$split__id <- NULL
	}
	
	
	list(co=co, gpl=gpl, shp=shp)
}
