process_shapes <- function(shps, g, gm, data_by, dw, dh) {
	
	sh <- (dh/gm$nrow) * (1-sum(gm$outer.margins[c(1,3)]))
	sw <- (dw/gm$ncol) * (1-sum(gm$outer.margins[c(2,4)]))
	
	dasp <- sw/sh
	pasp <- gm$asp
	if (identical(pasp, 0)) pasp <- dasp
	
	nx <- length(shps)
	shp_names <- sapply(g, function(i)i[[1]])
	names(shps) <- shp_names
	# find master
	masterID <- which(sapply(g, function(x)!is.null(x$projection) || !is.null(x$xlim) || !is.null(x$ylim) || !is.null(x$bbox)))
	
	if (length(masterID)>1) {
		warning("Multiple projections or bounding boxes defined. First one is taken.")
		masterID <- masterID[1]
	}
	if (!length(masterID)) masterID <- 1
	
	# get master shape and info
	shp <- shps[[masterID]]
	shp_name <- shp_names[masterID]
	projection <- g[[masterID]]$projection
	xlim <- g[[masterID]]$xlim
	ylim <- g[[masterID]]$ylim
	relative <- g[[masterID]]$relative
	bbox <- g[[masterID]]$bbox
	shp.proj <- attr(shp, "proj4string")@projargs
	
	if (is.na(shp.proj)) {
		warning(paste("Currect projection of shape", shp_name, "unknown. Long-lat (WGS84) is assumed."))
		shp.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
		attr(shp, "proj4string") <- CRS(shp.proj)
	}
	
	# edit and set projection
	isProjected <- !is.null(projection)
	if (isProjected) {
		if (is.raster(shp)) {
			warning("Unable to set projection for rasters")
			projection <- shp.proj
		} else {
			projection <- get_proj4_code(projection)
			shp <- spTransform(shp, CRS(projection))
		}
	} else {
		projection <- shp.proj
	}
	
	longlat <- !is_projected(shp)

	
	# set projection for other shapes
	shps <- mapply(function(x, shp_nm){
		if (shp_nm==shp_name) {
			x <- shp
		} else {
			x.proj <- attr(x, "proj4string")@projargs
			if (is.na(x.proj)) {
				if (maybe_longlat(attr(x, "bbox"))) {
					warning(paste("Currect projection of shape", shp_nm, "unknown. Long-lat (WGS84) is assumed."))
					x.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
				} else {
					warning(paste("Currect projection of shape", shp_nm, "unknown. The projection of", shp_name, "is assumed."))
					x.proj <- shp.proj
				}
				attr(x, "proj4string") <- CRS(x.proj)
			}
			if (x.proj != projection) {
				if (is.list(x)) {
					stop(paste("Raster", shp_nm, "has different projection and cannot be transformed"))
				}
				x <- spTransform(x, CRS(projection))
			}
		}
		x
	}, shps, names(shps), SIMPLIFY=FALSE)
	
	
	# define bounding box

	
	group_by <- any(gm$shp_nr != 0) && gm$free.coords
	group_split <- group_by && gm$drop.shapes
	
	if (group_by) {
		if (is.na(pasp)) pasp <- dasp
		
		shp_by_names <- gm$shp_name[gm$shp_nr != 0]
		data_by <- data_by[gm$shp_nr != 0]
		
		shps_by <- shps[gm$shp_nr != 0]
		
		nplots <- nlevels(data_by[[1]])
		
		shps_by_splt <- mapply(function(s_by, d_by) {
			if (inherits(s_by, "Spatial")) {
				split_shape(s_by, f = d_by)	
			}  else {
				split_raster(s_by, f = d_by)
			} 
		}, shps_by, data_by, SIMPLIFY=FALSE)
		
		
		shp.by.bbox <- sapply(shps_by, attr, which="bbox")
		shp.by.bbox <- matrix(c(apply(shp.by.bbox[1:2,,drop=FALSE], MARGIN = 1, min), apply(shp.by.bbox[3:4,,drop=FALSE], MARGIN = 1, max)),
							   nrow=2, dimnames=list(c("x", "y"), c("min", "max")))
		shp.by.bbox <- get_bbox_asp(shp.by.bbox, gm$inner.margins, longlat, pasp=NA)$bbox
		
		bboxes <- do.call("mapply", c(list(FUN=function(...){
			x <- list(...)
			
			bb <- sapply(x, attr, which="bbox")
			bb <- matrix(c(apply(bb[1:2,,drop=FALSE], MARGIN = 1, min), apply(bb[3:4,,drop=FALSE], MARGIN = 1, max)),
								  nrow=2, dimnames=list(c("x", "y"), c("min", "max")))
			
			bb[,1] <- pmax(bb[,1], shp.by.bbox[,1])
			bb[,2] <- pmin(bb[,2], shp.by.bbox[,2])
			
			bbox <- get_bbox_lim(bb, relative, bbox, xlim, ylim)
			
			bbox_asp <- get_bbox_asp(bbox, gm$inner.margins, longlat, pasp)$bbox
			
			if (bbox_asp[1,1] < shp.by.bbox[1,1]) bbox_asp[1,] <- bbox_asp[1, ] + (shp.by.bbox[1,1] - bbox_asp[1,1])
			if (bbox_asp[2,1] < shp.by.bbox[2,1]) bbox_asp[2,] <- bbox_asp[2, ] + (shp.by.bbox[2,1] - bbox_asp[2,1])
			
			if (bbox_asp[1,2] > shp.by.bbox[1,2]) bbox_asp[1,] <- bbox_asp[1, ] - (bbox_asp[1,2] - shp.by.bbox[1,2])
			if (bbox_asp[2,2] > shp.by.bbox[2,2]) bbox_asp[2,] <- bbox_asp[2, ] - (bbox_asp[2,2] - shp.by.bbox[2,2])
			bbox_asp
		}), shps_by_splt, list(SIMPLIFY=FALSE)))
		bb <- bboxes[[1]]

		sasp <- calc_asp_ratio(bb[1,], bb[2,], longlat)

	} else {
		shp.bbox <- attr(shp, "bbox")
		bbox <- get_bbox_lim(shp.bbox, relative, bbox, xlim, ylim)
		bbox_asp <- get_bbox_asp(bbox, gm$inner.margins, longlat, pasp)
		bb <- bbox_asp$bbox
		sasp <- bbox_asp$sasp
		
		#shp_by_name <- ""
	}

	if (group_split) {
		shps_by_ind <- ifelse(gm$shp_nr==0, 0, cumsum(gm$shp_nr!=0))
		shps2 <- lapply(1:nx, function(i){
			x <- if (gm$shp_nr[i]==0) {
				lapply(1:nplots, function(j) shps[[i]])
			} else {
				shps_by_splt[[shps_by_ind[i]]]
			}
			mapply(function(shp2, bb2){
				tryCatch({
					crop_shape(shp2, bbox=bb2)
				}, error = function(e) {
					attr(shp2, "bbox") <- bb2
					#cat("error\n")
					attr(shp2, "matchID") <- 1:length(shp2)
					shp2
				})
			}, x, bboxes, SIMPLIFY=FALSE)
		})
		
	} else {
	
		shps2 <- mapply(function(x, shp_nm){
			## try to crop the shape file at the bounding box in order to place bubbles and text labels inside the frame
			if (group_by) {
				lapply(bboxes, function(bb2){
					tryCatch({
						crop_shape(x, bbox=bb2)
					}, error = function(e) {
						attr(x, "bbox") <- bb2
						#cat("error\n")
						attr(x, "matchID") <- 1:length(x)
						x
					})
				})
			} else {
				tryCatch({
					crop_shape(x, bbox=bb)
				}, error = function(e) {
					attr(x, "bbox") <- bb
					#cat("error\n")
					attr(x, "matchID") <- 1:length(x)
					x
				})
			}
		}, shps, names(shps), SIMPLIFY=FALSE)
	
	}
	if (group_by) {
		shps2 <- lapply(1:nplots, function(i)lapply(shps2, function(j)j[[i]]))
	}
	
	
	if (inherits(shp, "Spatial")) {
		## determine automatic legend position based on polygon centers
		co <- if (inherits(shp, "SpatialLines")) {
			do.call("rbind", lapply(coordinates(shp), function(x) {
				do.call("rbind", x)	
			}))
		} else {
			coordinates(shp)
		}
		xn <- (co[,1]-bb[1])/(bb[3]-bb[1])
		yn <- (co[,2]-bb[2])/(bb[4]-bb[2])
		legend_pos <- which.max(c(
			min(sqrt((xn^2) + (yn^2))),
			min(sqrt((xn^2) + ((1-yn)^2))),
			min(sqrt(((1-xn)^2) + ((1-yn)^2))),
			min(sqrt(((xn-1)^2) + (yn^2)))))
	} else {
		legend_pos <- 2
	}
	
	attr(shps2, "sasp") <- ifelse(is.na(pasp), sasp, pasp)
	attr(shps2, "dasp") <- dasp
	attr(shps2, "legend_pos") <- legend_pos
	attr(shps2, "group_by") <- group_by
	shps2
}


get_bbox_lim <- function(shp.bbox, relative, bbox, xlim, ylim) {
	if (!is.null(bbox)) {
		bbox <- bbox
	} else {
		if (relative) {
			steps <- shp.bbox[, 2] - shp.bbox[, 1]
			xlim <- if (is.null(xlim)) {
				shp.bbox[1, ]
			} else {
				shp.bbox[1,1] + xlim * steps[1]
			}
			ylim <- if (is.null(ylim)) {
				shp.bbox[2, ]
			} else {
				shp.bbox[2,1] + ylim * steps[2]
			}
		} else {
			if (is.null(xlim)) xlim <- shp.bbox[1, ]
			if (is.null(ylim)) ylim <- shp.bbox[2, ]
		}
		bbox <- matrix(c(xlim, ylim), ncol = 2, byrow=TRUE, 
					   dimnames=list(c("x", "y"), c("min", "max")))
	}
	bbox
}


get_bbox_asp <- function(bbox, inner.margins, longlat, pasp) {
	# extend bounding box for asp ratio
	bbrange <- bbox[,2] - bbox[,1]
	
	xspan <- 1 - inner.margins[2] - inner.margins[4]
	yspan <- 1 - inner.margins[1] - inner.margins[3]
	
# 	inner.margins[c(2,4)] bbrange[1]
# 	
# 	bb <- bbox - 
# 	
# 	bbrange2 <- bbrange / c(xspan, yspan)
# 	bbmin <- bbox[,1] - 
	
	
	#bb <- bbox + rep(bbrange2, 2)
	
	bbmarg <- inner.margins[c(2,1,4,3)]
	bbmarg[c(1,2)] <- -bbmarg[c(1,2)]
	bb <- bbox + rep(bbrange/c(xspan, yspan), 2) * bbmarg
	
	xlim <- bb[1,]
	ylim <- bb[2,]
	
	sasp <- calc_asp_ratio(xlim, ylim, longlat)
	
	if (!is.na(pasp)) {
		if (pasp > sasp) {
			## landscape device
			xdiff <- if (longlat) diff(ylim) * pasp / cos((mean(ylim) * pi)/180) else diff(ylim) * (pasp)
			bb[1, ] <- mean(xlim) + (xdiff * c(-.5, .5))
		} else {
			## portrait device
			ydiff <- if (longlat) (diff(xlim) * cos((mean(ylim) * pi)/180)) / pasp else diff(xlim) / (pasp)
			bb[2, ] <- mean(ylim) + (ydiff * c(-.5, .5))
		}
	}
	list(bbox=bb, sasp=sasp)
}

split_raster <- function(r, f) {
	if (!is.factor(f)) {
		warning("f is not a factor")
		f <- as.factor(f)
	}
	bb <- attr(r, "bbox")
	lev <- intersect(levels(f), f)
	lapply(lev, function(l){
		m <- matrix(as.numeric(!is.na(f) & f==l), ncol=r@ncol, nrow=r@nrow, byrow = TRUE)
		cls <- colSums(m)
		rws <- rev(rowSums(m))
		
		xrng <- range(which(cls!=0))
		yrng <- range(which(rws!=0))
		
		xrng[1] <- xrng[1] - 1
		yrng[1] <- yrng[1] - 1
		
		xlim <- xrng / r@ncol
		ylim <- yrng / r@nrow
		
		attr(r, "bbox") <- matrix(c(bb[1,1] + (bb[1,2] - bb[1,1]) * xlim[1],
				 bb[1,1] + (bb[1,2] - bb[1,1]) * xlim[2],
				 bb[2,1] + (bb[2,2] - bb[2,1]) * ylim[1],
				 bb[2,1] + (bb[2,2] - bb[2,1]) * ylim[2]), ncol=2, dimnames=dimnames(bb), byrow = TRUE)
		r
	})
}
