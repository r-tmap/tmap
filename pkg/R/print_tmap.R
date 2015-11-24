#' Draw thematic map
#' 
#' Draw thematic map on current graphics device
#' 
#' @param x tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw the plot in. This is particularly useful for insets.
#' @param ... not used
#' @import sp
#' @importFrom raster raster extent setValues ncell couldBeLonLat fromDisk as.vector crop
#' @import RColorBrewer
#' @import grid
#' @import gridBase
#' @importFrom classInt classIntervals findCols
#' @importFrom rgeos gIntersection gIntersects gBuffer gDifference gCentroid
#' @importFrom grDevices col2rgb colorRampPalette dev.off is.raster png rgb
#' @importFrom methods as slot slotNames
#' @importFrom stats na.omit
#' @importFrom spdep poly2nb
#' @export
#' @method print tmap
print.tmap <- function(x, vp=NULL, ...) {
	#### General process of tmap:
	#  print.tmap: - puts shapes and shape data into right format
	#              - calls process_tm for processing tm elements
	#              - calls process_shapes for processing shapes
	#              - calls plot function gridplot, that calls plot_all
	#  process_tm: - get all non-layer elements, (tm_layout, tm_grid, ...)
	#              - process layer elements by calling process_layers (result is gp)
	#              - determine number of small multiples (nx)
	#              - process non-layer elements by calling process_meta (result is gmeta)
	#              - split gp into small multiples (result is gps)
	#  process_layers: - determine grouped small multiples (specified by user with tm_facets(by=))
	#                  - process layer functions by calling indivudual functions, like tm_fill
	#  process_meta: - determines number of rows and colums for small multiples
	#                - applies scale factor to all meta elements (tm_layout, tm_style, tm_compass, tm_scale_bar, tm_grid)
	#  process_shapes: - project shapes
	#                  - determines bounding box(es)
	#                  - crop shapes
	#  gridplot:   - makes small multiples grid
	#              - calls plot_all for grob trees
	#              - plot the grob trees
	#  plot_all:   - calls plot_map to create grob tree of map itself
	#              - calls legend_prepare and plot_legend to create grob tree of legend
	#              - creates grob tree for whole plot
	
	## identify shape blocks
	shape.id <- which(names(x)=="tm_shape")
	nshps <- length(shape.id)
	if (!nshps) stop("Required tm_shape layer missing.")
	
	## find "MAP_COLORING" values
	apply_map_coloring <- if ("tm_fill" %in% names(x)) {
		any(sapply(x[which(names(x)=="tm_fill")], function(i)identical(i$col[1],"MAP_COLORS")))
	} else FALSE
	
	
	## extract data.frames from shape/raster objects
	shps_dts <- lapply(x[shape.id], function(y) {
		shp <- y$shp
		shp.unit <- y$unit
		shp.unit.size <- y$unit.size
		
		if (inherits(shp, "Spatial")) {
			if (inherits(shp, "SpatialPixelsDataFrame")) {
				shp <- as(shp, "SpatialGridDataFrame")
			}
			## get data.frame from shapes, and store ID numbers in shape objects (needed for cropping)
			newData <- data.frame(tmapID = seq_len(length(shp)))
			if ("data" %in% slotNames(shp)) {
				data <- shp@data
				shp@data <- newData
				if (inherits(shp, "SpatialGridDataFrame")) {
					shp <- raster(shp, layer=0)
				}
			} else {
				data <- newData
				shp <- if (inherits(shp, "SpatialPolygons")) {
					SpatialPolygonsDataFrame(shp, data = newData, match.ID = FALSE)
				} else if (inherits(shp, "SpatialLines")) {
					SpatialLinesDataFrame(shp, data = newData, match.ID = FALSE)
				} else if (inherits(shp, "SpatialPoints")) {
					SpatialPointsDataFrame(shp, data = newData, match.ID = FALSE)
				} else if (inherits(shp, c("SpatialGrid", "SpatialPixels"))) {
					raster(shp, layer=0)
				}
			}
			
			if (inherits(shp, "SpatialPolygonsDataFrame")) {
				data$SHAPE_AREAS <- approx_areas(shp, unit=shp.unit, unit.size = shp.unit.size)
				attr(data, "AREAS_is_projected") <- is_projected(shp)
				if (apply_map_coloring) attr(data, "NB") <- if (length(shp)==1) list(0) else poly2nb(shp)
				attr(data, "dasymetric") <- ("dasymetric" %in% names(attributes(shp)))
				type <- "polygons"
			} else if (inherits(shp, "SpatialLinesDataFrame")) {
				attr(data, "isolines") <- ("isolines" %in% names(attributes(shp)))
				type <- "lines"
			} else if (inherits(shp, "SpatialPointsDataFrame")) {
				type <- "points"
			} else {
				type <- "raster"
			}
		} else if (inherits(shp, "Raster")) {
			if (fromDisk(shp)) {
				shp <- as(shp, "SpatialGridDataFrame")
				data <- data.frame(FILE__VALUES = shp[[1]])
				shp <- raster(shp, layer=0)
			} else {
				data <- get_raster_data(shp)
				shp <- raster(shp)
			}
			type <- "raster"
		} else {
			stop("Object ", y$shp_name, " is neither from class Spatial nor Raster.")
		}
		
		if (inherits(shp, "Raster")) {
			## convert to a RasterLayer with ID numbers
			shp <- setValues(shp, values=1:ncell(shp))
			
			## to be consistent with Spatial objects:
			attr(shp, "bbox") <- bbox(shp)
			attr(shp, "proj4string") <- shp@crs
		}
		attr(shp, "projected") <- is_projected(shp)
		
		list(shp=shp, data=data, type=type)
	})
	shps <- lapply(shps_dts, "[[", 1)
	datasets <- lapply(shps_dts, "[[", 2)
	types <- lapply(shps_dts, "[[", 3)
	
	## find master shape
	is_raster <- sapply(shps, inherits, "RasterLayer")
	is_master <- sapply(x[shape.id], "[[", "is.master")
	is_master <- is_master==TRUE & !is.na(is_master)
	
	any_raster <- any(is_raster)
	masterID <- if (any_raster) {
		if (any(is_master[is_raster])) which(is_raster)[is_master[is_raster]][1] else which(is_raster)[1]
	} else {
		if (any(is_master)) which(is_master)[1] else 1
	}
	
	## determine aspect ratio of master shape
	shpM_bb <- attr(shps[[masterID]], "bbox")
	shpM_asp <-	calc_asp_ratio(shpM_bb[1,], shpM_bb[2,], longlat=!attr(shps[[masterID]], "projected"))

	## remove shapes from and add data to tm_shape objects
	x[shape.id] <- mapply(function(y, dataset, type){
		#bb <- bbox(y$shp)
		y$type <- type
		y$data <- dataset
		y$shp <- NULL
		y
	}, x[shape.id], datasets, types, SIMPLIFY=FALSE)
	
	## prepare viewport
	if (is.null(vp)) {
		grid.newpage()
	} else {
		if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
	}
	
	## calculate device aspect ratio (needed for small multiples' nrow and ncol)
	inner.margins <- if ("tm_layout" %in% names(x)) {
		x[[which(names(x)=="tm_layout")[1]]]$inner.margins
	} else NA
	
	inner.margins <- if (is.na(inner.margins[1])) {
		if (any_raster) rep(0, 4) else rep(0.02, 4)
	} else rep(inner.margins, length.out=4)

	xmarg <- sum(inner.margins[c(2,4)])
	ymarg <- sum(inner.margins[c(1,3)])
	if (xmarg >= .8) stop("Inner margins too large")
	if (ymarg >= .8) stop("Inner margins too large")
	shpM_asp_marg <- shpM_asp * (1+(xmarg/(1-xmarg))) / (1+(ymarg/(1-ymarg)))
	dev_asp <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)/convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE)
	asp_ratio <- shpM_asp_marg / dev_asp
	
	## process tm objects
	shp_info <- x[[shape.id[masterID]]][c("unit", "unit.size", "line.center.type")]
	shp_info$is_raster <- any_raster
	result <- process_tm(x, asp_ratio, shp_info)
	gmeta <- result$gmeta
	
	gps <- result$gps
	nx <- result$nx
	data_by <- result$data_by
	
	## process shapes
	margins <- gmeta$outer.margins
	dw <- convertWidth(unit(1-sum(margins[c(2,4)]),"npc"), "inch", valueOnly=TRUE)
	dh <- convertHeight(unit(1-sum(margins[c(1,3)]),"npc"), "inch", valueOnly=TRUE)
	shps_lengths <- sapply(shps, length)
	shps <- process_shapes(shps, x[shape.id], gmeta, data_by, dw, dh, masterID)
	
	dasp <- attr(shps, "dasp")
	sasp <- attr(shps, "sasp")
	legend_pos <- attr(shps, "legend_pos")
	diff_shapes <- attr(shps, "diff_shapes")
	inner.margins.new <- attr(shps, "inner.margins")

	if (gmeta$design.mode) {
		masterShapeName <- x[[masterID]]$shp_name
		cat("aspect ratio device (yellow):", dev_asp, "\n")
		cat("aspect ratio frame (blue):", sasp, "\n")
		cat("aspect ratio master shape,", masterShapeName, "(red):", shpM_asp, "\n")
	}
	
	## shapes have been subset (diff_shapes) and cropped. Therefore, the corresponding aesthetics have to be subset accordingly:
	if (diff_shapes) {
		matchIDs <- lapply(shps, function(ss) lapply(ss, function(s) if (inherits(s, "Raster")) s[] else s$tmapID))
						   
		gps <- mapply(function(gp, masterID) {
			gp[1:nshps] <- mapply(function(gpl, indices, l) {
				gpl$npol <- length(indices)
				lapply(gpl, function(gplx) {
					if ((is.vector(gplx) || is.factor(gplx)) && length(gplx)==l) {
						gplx <- gplx[indices]	
					} else {
						gplx
					}
				})
			},  gp[1:nshps], masterID, shps_lengths, SIMPLIFY=FALSE)
			gp
		}, gps, matchIDs, SIMPLIFY=FALSE)
		
	} else {
		matchIDs <- lapply(shps, function(s) if (inherits(s, "Raster")) s[] else s$tmapID)

		gps <- lapply(gps, function(gp) {
			gp[1:nshps] <- mapply(function(gpl, indices, l) {
				gpl$npol <- length(indices)
				lapply(gpl, function(gplx) {
					if ((is.vector(gplx) || is.factor(gplx)) && length(gplx)==l) {
						gplx <- gplx[indices]	
					} else {
						gplx
					}
				})
			},  gp[1:nshps], matchIDs, shps_lengths, SIMPLIFY=FALSE)
			gp
		})
		
	}
	
	## create an environment to pass on large shapes, which is more efficient then passing on shapes themselves(is it??)
	shps.env <- environment()
	
	## plot
	gridplot(gmeta$nrow, gmeta$ncol, "plot_all", nx, gps, shps.env, dasp, sasp, inner.margins.new, legend_pos)
	
	## if vp is specified, go 1 viewport up, else go to root viewport
	upViewport(n=as.integer(!is.null(vp)))

	## return data
	vars <- unname(mapply(function(gp, p) {
		mapply(function(gpl, l) {
			lst <- list({if (!is.na(gpl$varnames$fill)[1]) {
				c(gpl$idnames$fill, gpl$varnames$fill)
			} else NULL}, 
			{if (!is.na(gpl$varnames$bubble.size)[1] || !is.na(gpl$varnames$bubble.size)[1]) {
				c(gpl$idnames$bubble, gpl$varnames$bubble.size, gpl$varnames$bubble.col, gpl$varnames$bubble.size)
			} else NULL},
			{if (!is.na(gpl$varnames$line.col)[1] || !is.na(gpl$varnames$line.lwd)[1]) {
				c(gpl$idnames$line, gpl$varnames$line.col, gpl$varnames$line.lwd)
			} else NULL})
			names(lst) <- paste("tm", c("polygons", "bubbles", "lines"), p, l, sep="_")
			lst
		}, gp[1:nshps], 1:nshps, SIMPLIFY=FALSE)
	}, gps, 1:nx, SIMPLIFY=FALSE))
	vars <- lapply(1:nshps, function(i){
		do.call("c", lapply(vars, "[[", i))
	})
	
	dat <- do.call("c", unname(mapply(function(d, v) {
		lapply(v, function(i) {
			if (is.null(i)) return(NULL)
			df <- subset(d, select=na.omit(i), drop=FALSE)
			if (!is.na(i[1])) names(df)[1] <- "ID"
			if (length(i)==4) {
				df <- df[order(df[i[4]], decreasing=TRUE), -ncol(df)]				
			}
			df
		})
	}, datasets, vars, SIMPLIFY=FALSE)))
	dat <- dat[!sapply(dat, is.null)]
	
	invisible(dat)
}


