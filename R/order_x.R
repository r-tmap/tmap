order_x <- function(x, shps, datasets, types, gm) {
	n <- length(x)
	y <- rep(0, n); y[gm$shape.id] <- 1
	
	cluster.id <- cumsum(y)
	xs <- split(x, cluster.id)
	xs_shp <- mapply(function(xp, shp, dataset, type, k) {
		from_sf <- ("from_tm_sf" %in% names(xp[[2]]))
		
		if (type == "geometrycollection") {
			tps <- attr(type, "types")
			cnts <- tabulate(tps, nbins = 3)
			
			xp_tiles <- xp[names(xp) == "tiles"]
			
			if (cnts[1]>0) {
				xp_poly <- 	xp[!(names(xp) %in% c("tm_lines", "tm_iso", "tm_raster", "tm_tiles"))]
				if (from_sf) xp_poly <- xp_poly[names(xp_poly) != "tm_symbols"]
				
				
				if (length(xp_poly) == 1) {
					xp_poly <- NULL
					shp_poly <- NULL
					cnts[1] <- 0
				} else {
					sel_poly <- which(tps == "polygons")
					shp_poly <- shp[sel_poly, ]
					shp_poly$tmapID <- seq_len(cnts[1])
					attr(shp_poly, "bbox") <- attr(shp, "bbox")
					attr(shp_poly, "proj4string") <- attr(shp, "proj4string")
					attr(shp_poly, "projected") <- attr(shp, "projected")
					attr(shp_poly, "point.per") <- attr(shp, "point.per")
					attr(shp_poly, "line.center") <- attr(shp, "line.center")
					
					
					data_poly <- dataset[sel_poly, , drop = FALSE]
					data_poly$SHAPE_AREAS <- tmaptools::approx_areas(shp=shp_poly, target = paste(gm$shape.unit, gm$shape.unit, sep=" "))
					if (gm$shape.apply_map_coloring) attr(data_poly, "NB") <- if (length(shp_poly)==1) list(0) else get_neighbours(shp_poly) #poly2nb(as(shp, "Spatial"))
					
					xp_poly[[1]]$type <- "polygons"
					xp_poly[[1]]$data <- data_poly
					xp_poly[[1]]$shp <- NULL
				}
			} else {
				xp_poly <- NULL
				shp_poly <- NULL
			}
			
			if (cnts[2]>0) {
				xp_lines <- xp[!(names(xp) %in% c("tm_fill", "tm_borders", "tm_raster", "tm_tiles"))]
				if (from_sf) xp_lines <- xp_lines[names(xp_lines) != "tm_symbols"]
				
				if (length(xp_lines) == 1) {
					xp_lines <- NULL
					shp_lines <- NULL
					cnts[2] <- 0
				} else {
					sel_lines <- which(tps == "lines")
					shp_lines <- shp[sel_lines, ]
					shp_lines$tmapID <- seq_len(cnts[2])
					attr(shp_lines, "bbox") <- attr(shp, "bbox")
					attr(shp_lines, "proj4string") <- attr(shp, "proj4string")
					attr(shp_lines, "projected") <- attr(shp, "projected")
					attr(shp_lines, "point.per") <- attr(shp, "point.per")
					attr(shp_lines, "line.center") <- attr(shp, "line.center")
					xp_lines[[1]]$type <- "lines"
					xp_lines[[1]]$data <- dataset[sel_lines, , drop = FALSE]
					xp_lines[[1]]$shp <- NULL
				}
			} else {
				xp_lines <- NULL
				shp_lines <- NULL
			}
			
			if (cnts[3]>0) {
				xp_points <- xp[!(names(xp) %in% c("tm_fill", "tm_borders", "tm_lines", "tm_iso", "tm_raster", "tm_tiles"))]
				if (length(xp_points) == 1) {
					xp_points <- NULL
					shp_points <- NULL
					cnts[3] <- 0
				} else {
					sel_points <- which(tps == "points")
					shp_points <- shp[sel_points, ]
					shp_points$tmapID <- seq_len(cnts[3])
					attr(shp_points, "bbox") <- attr(shp, "bbox")
					attr(shp_points, "proj4string") <- attr(shp, "proj4string")
					attr(shp_points, "projected") <- attr(shp, "projected")
					attr(shp_points, "point.per") <- attr(shp, "point.per")
					attr(shp_points, "line.center") <- attr(shp, "line.center")
					
					xp_points[[1]]$type <- "points"
					xp_points[[1]]$data <- dataset[sel_points, , drop = FALSE]
					xp_points[[1]]$shp <- NULL
				}
			} else {
				xp_points <- NULL
				shp_points <- NULL
			}
			
			xp <- c(xp_poly, xp_lines, xp_points)
			shp <- list(shp_poly, shp_lines, shp_points)
			shp <- shp[!vapply(shp, is.null, logical(1))]
			k <- rep(k, sum(cnts>0))
		} else {
			
			# subset elements when tm_sf is called
			if (("tm_fill" %in% names(xp)) && from_sf) {
				if (type == "polygons") {
					xp <- xp[c("tm_shape", "tm_fill", "tm_borders", "tm_tiles")]
				} else if (type == "lines") {
					xp <- xp[c("tm_shape", "tm_lines", "tm_tiles")]
				} else if (type == "points") {
					xp <- xp[c("tm_shape", "tm_symbols", "tm_tiles")]
				}
			}
			xp[[1]]$type <- type
			xp[[1]]$data <- dataset
			xp[[1]]$shp <- NULL
			shp <- list(shp)
		}
		attr(xp, "k") <- k
		attr(xp, "names") <- names(xp)
		list(xp=xp, shp=shp)
	}, xs, shps, datasets, types, 1:length(types), SIMPLIFY = FALSE)
	
	xs2 <- lapply(xs_shp, "[[", 1)
	shps2 <- do.call(c, lapply(xs_shp, "[[", 2))
	
	k <- unname(unlist(lapply(xs2, attr, "k")))
	
	nms <- unname(unlist(lapply(xs2, attr, "names")))
	
	gm$shape.id <- unname(which(nms == "tm_shape"))
	gm$shape.nshps <- length(gm$shape.id)
	gm$shape.masterID <- which(k == gm$shape.masterID)[1]
	
	x2 <- do.call(c, c(xs2, list(use.names = FALSE)))
	names(x2) <- nms
	
	list(x=x2, shps=shps2, gm=gm)
}
