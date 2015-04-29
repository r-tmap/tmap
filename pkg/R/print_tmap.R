#' Draw tmap plot
#' 
#' Draw tmap plot on current graphics device
#' 
#' @param x tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw the plot in
#' @param ... not used
#' @import sp
#' @import raster
#' @import RColorBrewer
#' @import grid
#' @import gridBase
#' @import classInt
#' @export
#' @method print tmap
print.tmap <- function(x, vp=NULL, ...) {
	## identify shape blocks
	shape.id <- which(names(x)=="tm_shape")
	nshps <- length(shape.id)
	if (!nshps) stop("Required tm_shape layer missing.")
	


	## split shapes from data 
	## so a SPDF is split into a SpatialPolygons and a seperate data.frame
	## for raster objects (also SpatialGrid and Pixels), a RasterLayer is kept
	shps_dts <- lapply(x[shape.id], function(y) {
		shp <- y$shp
		if (inherits(shp, "Spatial")) {
			if ("data" %in% names(attributes(shp))) {
				data <- shp@data
				isPixels <- inherits(shp, "SpatialPixelsDataFrame")
				shp <- if (inherits(shp, "SpatialPolygonsDataFrame")) {
					as(shp, "SpatialPolygons")
				} else if (inherits(shp, "SpatialLinesDataFrame")) {
					as(shp, "SpatialLines")
				} else if (inherits(shp, "SpatialGridDataFrame")) {
					shp@data <- data.frame(ID=1:nrow(data))
					as(shp, "RasterLayer")
				} else if (inherits(shp, "SpatialPixelsDataFrame")) {
					shp@data <- data.frame(ID=1:nrow(data))
					as(shp, "RasterLayer")
				} else if (inherits(shp, "SpatialPointsDataFrame")) {
					as(shp, "SpatialPoints")
				}
				if (isPixels) {
					data <- data[shp@data@values, ,drop=FALSE]
				}
			} else {
				data <- data.frame(ID=get_IDs(shp))
			}
			if (inherits(shp, "SpatialPolygons")) {
				data$SHAPE_AREAS <- approx_areas(shp, units="abs") / 1e6
			}
		} else if (inherits(shp, "Raster")) {
			data <- get_Raster_data(shp)
		}
			
		
		if (inherits(shp, "Raster")) {
			bb <- bbox(shp)
			crs <- shp@crs
			projected <- is_projected(shp)
			#shp <- as.raster(shp)
			#shp <- list(ncols=shp@ncols, nrows=shp@nrows, rotated=shp@rotated)
			attr(shp, "bbox_raster") <- bb
			attr(shp, "bbox") <- bb
			attr(shp, "proj4string") <- crs
			attr(shp, "projected") <- projected
		}
		list(shp=shp, data=data)
	})

	shps <- lapply(shps_dts, "[[", 1)
	datasets <- lapply(shps_dts, "[[", 2)

	shp1_bb <- attr(shps[[1]], "bbox")
	shp1_asp <-	calc_asp_ratio(shp1_bb[1,], shp1_bb[2,], longlat=!is_projected(shps[[1]]))

	x[shape.id] <- mapply(function(y, dataset){
		#bb <- bbox(y$shp)
		y$data <- dataset
		y$shp <- NULL
		y
	}, x[shape.id], datasets, SIMPLIFY=FALSE)
	
	if (is.null(vp)) {
		grid.newpage()
	} else {
		if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
	}
	
	inner.margins <- if ("tm_layout" %in% names(x)) {
		rep(x[[which(names(x)=="tm_layout")[1]]]$inner.margins, length.out=4)
	} else {
		inner.margins <- rep(.02, 4)	
	}
	xmarg <- sum(inner.margins[c(2,4)])
	ymarg <- sum(inner.margins[c(1,3)])
	
	if (xmarg >= .8) stop("Inner margins too large")
	if (ymarg >= .8) stop("Inner margins too large")
	
	shp1_asp_marg <- shp1_asp * (1+(xmarg/(1-xmarg))) / (1+(ymarg/(1-ymarg)))
	
	dev_asp <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE) / convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE)
	
	asp_ratio <- shp1_asp_marg / dev_asp
	
	result <- process_tm(x, asp_ratio)
	
	gmeta <- result$gmeta
	gps <- result$gps
	nx <- result$nx
	data_by <- result$data_by
	
	
	margins <- gmeta$outer.margins
	dw <- convertWidth(unit(1-sum(margins[c(2,4)]),"npc"), "inch", valueOnly=TRUE)
	dh <- convertHeight(unit(1-sum(margins[c(1,3)]),"npc"), "inch", valueOnly=TRUE)
	
	shps_lengths <- sapply(shps, length)
	shps <- process_shapes(shps, x[shape.id], gmeta, data_by, dw, dh)
	
	dasp <- attr(shps, "dasp")
	sasp <- attr(shps, "sasp")
	legend_pos <- attr(shps, "legend_pos")
	group_by <- attr(shps, "group_by")
	
	
	## unify projections and set bounding box
	if (group_by) {
		matchIDs <- lapply(shps, function(ss) lapply(ss, function(s)attr(s, "matchID")))
		
		gps <- mapply(function(gp, mID) {
			gp[1:nshps] <- mapply(function(gpl, indices, l) {
				gpl$npol <- length(indices)
				lapply(gpl, function(gplx) {
					if ((is.vector(gplx) || is.factor(gplx)) && length(gplx)==l) {
						gplx <- gplx[indices]	
					} else {
						gplx
					}
				})
			},  gp[1:nshps], mID, shps_lengths, SIMPLIFY=FALSE)
			gp
		}, gps, matchIDs, SIMPLIFY=FALSE)
		
	} else {
		matchIDs <- lapply(shps, function(s)attr(s, "matchID"))

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
	
	
	shps.env <- environment()#new.env()
	#assign("shps", shps, envir=shps.env)
	
	gridplot(gmeta$nrow, gmeta$ncol, "plot_all", nx, gps, shps.env, dasp, sasp, legend_pos)
	invisible()
}


get_RasterLayer_data_vector <- function(r) {
	values <- r@data@values
	if (r@data@isfactor) {
		dt <- r@data@attributes[[1]]
		if ("levels" %in% names(dt)) {
			factor(values, levels=dt$ID, labels=dt$levels)
		} else {
			warning("No 'levels' column found in data@attributes.")
			values
		}
	} else {
		values
	}
}


get_Raster_data <- function(shp) {
	if (inherits(shp, "RasterLayer")) {
		data <- data.frame(get_RasterLayer_data_vector(shp))
		names(data) <- names(shp)
	} else if (inherits(shp, "RasterStack")) {
		data <- as.data.frame(lapply(shp@layers, get_RasterLayer_data_vector))
		names(data) <- names(shp)
	} else if (inherits(shp, "RasterBrick")) {
		isfactor <- shp@data@isfactor
		data <- as.data.frame(shp@data@values)
		atb <- shp@data@attributes
		atb <- atb[sapply(atb, length)!=0]
		
		stopifnot(sum(isfactor)==length(atb))
		
		if (any(isfactor)) data[isfactor] <- mapply(function(d, a){
			if (class(a)=="list") a <- a[[1]]
			levelsID <- ncol(a) # levels is always the last column of the attributes data.frame (?)
			factor(d, levels=a$ID, labels=as.character(a[[levelsID]]))
		}, data[isfactor], atb, SIMPLIFY=FALSE)
	}	
	data
}