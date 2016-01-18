#' Draw thematic map
#' 
#' Draw thematic map on current graphics device
#' 
#' @param x tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw the plot in. This is particularly useful for insets.
#' @param return.asp Logical that determines whether the aspect ratio of the map is returned. In that case, \code{\link[grid:grid.newpage]{grid.newpage()}} will be called, but without plotting of the map. This is used by \code{\link{save_tmap}} to determine the aspect ratio of the map.
#' @param plot should \code{x} be plot?
#' @param interactive should the plot be prepared for interaction? If \code{TRUE}, latitude longitude ( WGS84) coordinates are used and margins will be set to 0.
#' @param ... not used
#' @return A list of data.frames is silently returned, containing all ID and aesthetic variables per layer group.
#' @import sp
#' @importFrom raster raster extent setValues ncell couldBeLonLat fromDisk crop
#' @importMethodsFrom raster as.vector
#' @import RColorBrewer
#' @import grid
#' @importFrom classInt classIntervals findCols
#' @importFrom rgeos gIntersection gIntersects gBuffer gDifference gCentroid
#' @importFrom grDevices col2rgb colorRampPalette dev.off is.raster png rgb
#' @importFrom methods as slot slotNames is
#' @importFrom stats na.omit dnorm fft quantile rnorm runif 
#' @importFrom spdep poly2nb
#' @importFrom grDevices xy.coords
#' @importFrom graphics par
#' @importFrom rgdal getPROJ4VersionInfo
#' @importFrom utils capture.output data download.file head setTxtProgressBar tail txtProgressBar
#' @importMethodsFrom raster as.vector
#' @importFrom geosphere distGeo
#' @import leaflet
#' @importFrom htmltools htmlEscape
#' @export
#' @method print tmap
print.tmap <- function(x, vp=NULL, return.asp=FALSE, mode=getOption("tmap.mode"), ...) {
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
	interactive <- (mode == "view")
	
	## identify shape blocks
	shape.id <- which(names(x)=="tm_shape")
	nshps <- length(shape.id)
	if (!nshps) stop("Required tm_shape layer missing.", call. = FALSE)
	
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
			stop("Object ", y$shp_name, " is neither from class Spatial nor Raster.", call. = FALSE)
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
	
	# remove facets if interactive
	if (interactive) {
		x[names(x)=="tm_facets"] <- NULL
	}

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
	
	if (interactive) {
		x[shape.id[masterID]]$tm_shape$projection <- get_proj4("longlat")
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
	if (interactive) {
		asp_ratio <- 1
	} else {
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
		if (xmarg >= .8) stop("Inner margins too large", call. = FALSE)
		if (ymarg >= .8) stop("Inner margins too large", call. = FALSE)
		shpM_asp_marg <- shpM_asp * (1+(xmarg/(1-xmarg))) / (1+(ymarg/(1-ymarg)))
		dev_asp <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)/convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE)
		asp_ratio <- shpM_asp_marg / dev_asp
	}
	
	
	## process tm objects
	shp_info <- x[[shape.id[masterID]]][c("unit", "unit.size", "line.center.type")]
	shp_info$is_raster <- any_raster
	result <- process_tm(x, asp_ratio, shp_info, interactive)
	gmeta <- result$gmeta
	
	# overrule margins if interactive
	if (interactive) {
		gmeta$inner.margins <- rep(0, 4)
		gmeta$outer.margins <- rep(0, 4)
		gmeta$asp <- NA
	}
	
	gps <- result$gps
	nx <- result$nx
	data_by <- result$data_by
	
	## process shapes
	margins <- gmeta$outer.margins
	dw <- convertWidth(unit(1-sum(margins[c(2,4)]),"npc"), "inch", valueOnly=TRUE)
	dh <- convertHeight(unit(1-sum(margins[c(1,3)]),"npc"), "inch", valueOnly=TRUE)
	shps_lengths <- sapply(shps, length)

	shps <- process_shapes(shps, x[shape.id], gmeta, data_by, dw, dh, masterID, allow.crop = !interactive, raster.leaflet=interactive)
	
	dasp <- attr(shps, "dasp")
	sasp <- attr(shps, "sasp")
	legend_pos <- attr(shps, "legend_pos")
	diff_shapes <- attr(shps, "diff_shapes")
	inner.margins.new <- attr(shps, "inner.margins")

	# shortcut used by save_tmap
	if (return.asp) {
		return(sasp)
	}
	
	if (gmeta$design.mode && !interactive) {
		masterShapeName <- x[[masterID]]$shp_name
		message("aspect ratio device (yellow):", dev_asp)
		message("aspect ratio frame (blue):", sasp)
		message("aspect ratio master shape,", masterShapeName, "(red):", shpM_asp)
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
	
	# apped data to gps
	gps2 <- lapply(gps, function(gp) {
		gp[-length(gp)] <- mapply(function(gpl, dt) {
			if (interactive) {
				if (!is.na(gpl$xfill)) {
					gpl$fill.values <- dt[[gpl$xfill]]
					if (!is.na(gpl$idnames$fill)) {
						gpl$fill.names <- dt[[gpl$idnames$fill]]
					}
				}
				if (!is.na(gpl$xsize) || !is.na(gpl$xcol)) {
					if (!is.na(gpl$xsize)) {
						gpl$bubble.size.values <- dt[[gpl$xsize]]
					}
					if (!is.na(gpl$xcol)) {
						gpl$bubble.col.values <- dt[[gpl$xcol]]
					}
					if (!is.na(gpl$idnames$bubble)) {
						gpl$bubble.names <- dt[[gpl$idnames$bubble]]
					}
				}
				if (!is.na(gpl$xline) || !is.na(gpl$xlinelwd)) {
					if (!is.na(gpl$xline)) {
						gpl$line.col.values <- dt[[gpl$xline]]
					}
					if (!is.na(gpl$xlinelwd)) {
						gpl$line.lwd.values <- dt[[gpl$xlinelwd]]
					}
					if (!is.na(gpl$idnames$line)) {
						gpl$line.names <- dt[[gpl$idnames$line]]
					}
				}
				if (!is.na(gpl$xraster)) {
					gpl$raster.values <- dt[[gpl$xraster]]
				}
				dt$SHAPE_AREAS <- NULL
			}
			gpl$data <- dt
			gpl
		}, gp[-length(gp)], datasets, SIMPLIFY = FALSE)
		gp
	})
	
	## plot
	if (interactive) {
		print(view_tmap(gps2, shps))
	} else {
		gridplot(gmeta$nrow, gmeta$ncol, "plot_all", nx, gps, shps.env, dasp, sasp, inner.margins.new, legend_pos)
		
		## if vp is specified, go 1 viewport up, else go to root viewport
		upViewport(n=as.integer(!is.null(vp)))
		invisible(list(shps=shps, gps=gps2))
	}
}


