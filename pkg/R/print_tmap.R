#' Draw thematic map
#' 
#' Draw thematic map on current graphics device
#' 
#' @param x tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw the plot in. This is particularly useful for insets.
#' @param ... not used
#' @import sp
#' @import raster
#' @import RColorBrewer
#' @import grid
#' @import gridBase
#' @import classInt
#' @import rgeos
#' @export
#' @method print tmap
print.tmap <- function(x, vp=NULL, ...) {
	## identify shape blocks
	shape.id <- which(names(x)=="tm_shape")
	nshps <- length(shape.id)
	if (!nshps) stop("Required tm_shape layer missing.")
	
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
			if ("data" %in% names(attributes(shp))) {
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
			}
		} else if (inherits(shp, "Raster")) {
			if (fromDisk(shp)) {
				system.time({
					shp <- as(shp, "SpatialGridDataFrame")
					data <- data.frame(FILE__VALUES = shp[[1]])
					shp <- raster(shp, layer=0)
				})
			} else {
				data <- get_raster_data(shp)
				shp <- raster(shp)
			}
		}
		
		if (inherits(shp, "Raster")) {
			## convert to a RasterLayer with ID numbers
			shp <- setValues(shp, values=1:ncell(shp))
			
			## to be consistent with Spatial objects:
			attr(shp, "bbox") <- bbox(shp)
			attr(shp, "proj4string") <- shp@crs
		}
		attr(shp, "projected") <- is_projected(shp)
		list(shp=shp, data=data)
	})
	shps <- lapply(shps_dts, "[[", 1)
	datasets <- lapply(shps_dts, "[[", 2)
	
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
	x[shape.id] <- mapply(function(y, dataset){
		#bb <- bbox(y$shp)
		y$data <- dataset
		y$shp <- NULL
		y
	}, x[shape.id], datasets, SIMPLIFY=FALSE)
	
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
	shp_info <- x[[shape.id[masterID]]][c("unit", "unit.size")]
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
	group_by <- attr(shps, "group_by")
	inner.margins.new <- attr(shps, "inner.margins")
	
	
	if (gmeta$design.mode) {
		masterShapeName <- x[[masterID]]$shp_name
		cat("aspect ratio device (yellow):", dev_asp, "\n")
		cat("aspect ratio frame (blue):", sasp, "\n")
		cat("aspect ratio master shape,", masterShapeName, "(red):", shpM_asp, "\n")
	}
	
	## shapes have been subset (group_by) and cropped. Therefore, the corresponding aesthetics have to be subset accordingly:
	if (group_by) {
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

	invisible()
}


