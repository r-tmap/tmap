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
			co <- suppressWarnings(st_coordinates(st_geometry(st_centroid(shp))))
		}
	} else if (st_geometry_type(shp)[1] %in% c("POLYGON", "MULTIPOLYGON"))  {
		co <- suppressWarnings(st_coordinates(st_geometry(st_centroid(shp, of_largest_polygon = (gt$shape.point.per == "largest")))))
	} else {
		co <- suppressWarnings(st_coordinates(st_geometry(st_centroid(shp))))
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

get_sp_coordinates <- function(shp, gpl, gt, bbx) {
	if (inherits(shp, "SpatialLines")) {
		
		if (gt$shape.line.center.type[1]=="segment") {
			ns <- length(shp)
			shp_lst <- one_line_per_lines(shp)
			shp <- shp_lst$shp
			attr(shp, "bbox") <- bbx
			id <- shp_lst$id
			
			aes <- intersect(names(gpl), c("line.col", "line.lwd", "text", "text.size", "text.color", "text.xmod", "text.ymod", "text_sel", "symbol.size", "symbol.col", "symbol.xmod", "symbol.ymod"))
			
			gpl[aes] <- lapply(gpl[aes], function(a) {
				if (length(a)==ns) {
					a[id]
				} else a
			})
		}
		
		if (gt$shape.line.center.type[2]=="midpoint") {
			co <- lines_midpoints(shp)@coords
			
			
			
			
			
			
			if (gt$shape.line.center.type[1]=="feature") {
				lC <- gCentroid(shp, byid=TRUE)@coords
				lCX <- lC[,1]
				lCY <- lC[,2]
				lens <- sapply(shp@lines, function(lns)length(lns@Lines))
				
				X <- co[,1]
				Y <- co[,2]
				ID <- do.call("c", mapply(rep, 1:length(lens), lens, SIMPLIFY=FALSE))
				Xs <- split(X, ID)
				Ys <- split(Y, ID)
				
				co <- t(mapply(function(x1, y1, x2, y2) {
					minid <- which.min(sqrt((x1-x2)^2 + (y1-y2)^2))
					c(x2[minid], y2[minid])
				}, lCX, lCY, Xs, Ys, SIMPLIFY=TRUE))
			}
		} else {
			co <- gCentroid(shp, byid=TRUE)@coords
		}
		
	} else {
		co <- coordinates(shp) # prefered over gCentroid since coordinates correspond to first (normally largest) polygon of each object
	}
	list(co=co, gpl=gpl, shp=shp)
}