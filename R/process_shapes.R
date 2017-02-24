process_shapes <- function(shps, g, gm, data_by, allow.crop, interactive) {
	
	pasp <- gm$asp
	if (gm$legend.only) pasp <- 0
	if (identical(pasp, 0)) pasp <- gm$shape.lasp
	
	nx <- length(shps)
	shp_names <- sapply(g, function(i)i[[1]])
	names(shps) <- shp_names
	
	# get master shape and info
	shp <- shps[[gm$shape.masterID]]
	args <- gm$shape.bb_args
	
	# in case x is search query
	if (!is.null(args$x)) {
		if (is.character(args$x)) {
			args$projection <- gm$shape.master_CRS
			args$current.projection <- .CRS_longlat
		} else if (interactive) {
			args$projection <- gm$shape.master_CRS
			args$current.projection <- gm$shape.orig_CRS
		} else {
			args$projection <- NULL
			args$current.projection <- NULL
		}
	}

	# define bounding box
	longlat <- !tmaptools::is_projected(shp)
	
	group_by <- any(gm$shp_nr != 0)
	
	free_coords <- group_by && gm$free.coords
	drop_shapes <- group_by && gm$drop.units
	diff_shapes <- free_coords || drop_shapes
	
	inside_bbox <- group_by && gm$inside.original.bbox
	if (diff_shapes) {
		if (is.na(pasp)) pasp <- gm$shape.lasp
		
		#shp_by_names <- gm$shp_name[gm$shp_nr != 0]
		data_by <- data_by[gm$shp_nr != 0]
		
		shps_by <- shps[gm$shp_nr != 0]
		
		nplots <- nlevels(data_by[[1]])
		
		shps_by_splt <- mapply(function(s_by, d_by) {
			if (inherits(s_by, "Spatial")) {
				split(s_by, f = d_by, drop=FALSE)	 # split_shape
			}  else {
				split_raster(s_by, f = d_by, drop=FALSE)
			} 
		}, shps_by, data_by, SIMPLIFY=FALSE)
	}
	
	if (free_coords) {
		# find maximum bbox
		shp.by.bbox <- sapply(shps_by, attr, which="bbox")
		shp.by.bbox <- matrix(c(apply(shp.by.bbox[1:2,,drop=FALSE], MARGIN = 1, min), apply(shp.by.bbox[3:4,,drop=FALSE], MARGIN = 1, max)),
							   nrow=2, dimnames=list(c("x", "y"), c("min", "max")))
		shp.by.bbox <- get_bbox_asp(shp.by.bbox, gm$inner.margins, longlat, pasp=NA, interactive=interactive)$bbox
		bboxes <- do.call("mapply", c(list(FUN=function(...){
			x <- list(...)
			
			bbx <- sapply(x, attr, which="bbox")
			if (is.null(bbx[[1]])) return(NULL)
			bbx <- matrix(c(apply(bbx[1:2,,drop=FALSE], MARGIN = 1, min), apply(bbx[3:4,,drop=FALSE], MARGIN = 1, max)),
								  nrow=2, dimnames=list(c("x", "y"), c("min", "max")))
			
			if (inside_bbox) {
				bbx[,1] <- pmax(bbx[,1], shp.by.bbox[,1])
				bbx[,2] <- pmin(bbx[,2], shp.by.bbox[,2])
			}
			if (!("x" %in% names(args))) args$x <- bbx
			bbox <- do.call("bb", args)  #get_bbox_lim(bbx, relative, bbox, xlim, ylim, ext)
			bbox_asp <- get_bbox_asp(bbox, gm$inner.margins, longlat, pasp, interactive=interactive)$bbox
			
			if (inside_bbox) {
				if (bbox_asp[1,1] < shp.by.bbox[1,1]) bbox_asp[1,] <- bbox_asp[1, ] + (shp.by.bbox[1,1] - bbox_asp[1,1])
				if (bbox_asp[2,1] < shp.by.bbox[2,1]) bbox_asp[2,] <- bbox_asp[2, ] + (shp.by.bbox[2,1] - bbox_asp[2,1])
				
				if (bbox_asp[1,2] > shp.by.bbox[1,2]) bbox_asp[1,] <- bbox_asp[1, ] - (bbox_asp[1,2] - shp.by.bbox[1,2])
				if (bbox_asp[2,2] > shp.by.bbox[2,2]) bbox_asp[2,] <- bbox_asp[2, ] - (bbox_asp[2,2] - shp.by.bbox[2,2])
			}
			bbox_asp
		}), shps_by_splt, list(SIMPLIFY=FALSE)))
		bbx <- bboxes[[1]]

		sasp <- get_asp_ratio(bbx, is.projected = !longlat)
		inner.margins <- gm$inner.margins
		
	} else {
		shp.bbox <- attr(shp, "bbox")
		if (!("x" %in% names(args))) args$x <- shp.bbox
		
		bbox <- do.call("bb", args)  #bbox <- get_bbox_lim(shp.bbox, relative, bbox, xlim, ylim, ext)
		bbox_asp <- get_bbox_asp(bbox, gm$inner.margins, longlat, pasp, interactive=interactive)
		bbx <- bbox_asp$bbox
		if (drop_shapes) bboxes <- rep(list(bbx), nplots)
		sasp <- bbox_asp$sasp
		inner.margins <- bbox_asp$inner.margins
		#shp_by_name <- ""
	}

	if (drop_shapes) {
		shps_by_ind <- ifelse(gm$shp_nr==0, 0, cumsum(gm$shp_nr!=0))
		shps2 <- lapply(1:nx, function(i){
			x <- if (gm$shp_nr[i]==0) {
				lapply(1:nplots, function(j) shps[[i]])
			} else {
				shps_by_splt[[shps_by_ind[i]]]
			}
			mapply(function(shp2, bb2){
				if (is.null(shp2)) return(NULL)
				if (!allow.crop) {
					attr(shp2, "bbox") <- bb2
					return(shp2)
				}
				prj <- attr(shp2, "proj4string")
				y <- tryCatch({
					crop(shp2, bb(bb2, ext=-1.01))
				}, error = function(e) {
					#cat("error\n")
					shp2
				})
				if (is.null(y)) y <- shp2
				attr(y, "bbox") <- bb2
				attr(y, "proj4string") <- prj
				y
			}, x, bboxes, SIMPLIFY=FALSE)
		})
		
	} else {
	
		shps2 <- mapply(function(x, shp_nm){
			if (is.null(x)) return(NULL)
			
			## try to crop the shape file at the bounding box in order to place symbols and text labels inside the frame. Use a little wider bounding box to prevent polygons following cropbb(bbx, ext=-1.01)
			if (diff_shapes) {
				lapply(bboxes, function(bb2){
					if (is.null(bb2)) return(NULL)
					if (!allow.crop) {
						attr(x, "bbox") <- bb2
						return(x)
					}
					prj <- attr(x, "proj4string")
					y <- crop(x, bb(bb2, ext=-1.01))
					if (is.null(y)) y <- x
					attr(y, "bbox") <- bb2
					attr(y, "proj4string") <- prj
					y
				})
			} else {
				if (!allow.crop) {
					attr(x, "bbox") <- bbx
					return(x)
				}
				prj <- attr(x, "proj4string")
				y <- tryCatch({
					y <- crop_shape(x, bb(bbx, ext=-1.01))
					if (is.null(y)) x else y
				}, error=function(e) {
					x	
				})
				attr(y, "bbox") <- bbx
				attr(y, "proj4string") <- prj
				y	
			}
		}, shps, names(shps), SIMPLIFY=FALSE)
	
	}
	if (diff_shapes) {
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
		xn <- (co[,1]-bbx[1])/(bbx[3]-bbx[1])
		yn <- (co[,2]-bbx[2])/(bbx[4]-bbx[2])
		legend_pos <- which.max(c(
			min(sqrt((xn^2) + (yn^2))),
			min(sqrt((xn^2) + ((1-yn)^2))),
			min(sqrt(((1-xn)^2) + ((1-yn)^2))),
			min(sqrt(((xn-1)^2) + (yn^2)))))
	} else {
		legend_pos <- 2
	}

	units <- do.call(tmaptools::projection_units, c(list(x=gm$shape.master_CRS, latitude=mean(bbx[c(2,4)])), gm$shape.units_args))

	#units <- tmaptools::get_shape_units(projection=gm$shape.master_CRS, latitude=mean(bbx[c(2,4)]), target.unit = gm$shape.units_args$unit)
	
	attr(shps2, "info") <-
		list(shape.sasp = ifelse(is.na(pasp), sasp, pasp),
			 shape.bbx = bbx,
			 shape.legend_pos = legend_pos,
			 shape.diff_shapes = diff_shapes,
			 shape.inner.margins = inner.margins,
			 shape.units=units)
	shps2
}



get_bbox_asp <- function(bbox, inner.margins, longlat, pasp, interactive) {
	
	# extend bounding box for asp ratio
	bbrange <- bbox[,2] - bbox[,1]
	
	xspan <- 1 - inner.margins[2] - inner.margins[4]
	yspan <- 1 - inner.margins[1] - inner.margins[3]
	
	bbmarg <- inner.margins[c(2,1,4,3)]
	bbmarg[c(1,2)] <- -bbmarg[c(1,2)]
	bbx <- bbox + rep(bbrange/c(xspan, yspan), 2) * bbmarg
	
	xlim <- bbx[1,]
	ylim <- bbx[2,]
	
	sasp <- get_asp_ratio(bbx, is.projected = !longlat)
	
	if (interactive) return(list(bbox=bbox, sasp=sasp, inner.margins= rep(0,4)))
	
	if (!is.na(pasp)) {
		if (pasp > sasp) {
			## landscape map
			xdiff <- if (longlat) diff(ylim) * pasp / cos((mean(ylim) * pi)/180) else diff(ylim) * (pasp)
			bbx[1, ] <- mean(xlim) + (xdiff * c(-.5, .5))
			
		} else {
			## portrait map
			ydiff <- if (longlat) (diff(xlim) * cos((mean(ylim) * pi)/180)) / pasp else diff(xlim) / (pasp)
			bbx[2, ] <- mean(ylim) + (ydiff * c(-.5, .5))
		}
	}
	
	# recalculate inner.margins (needed for design.mode)
	bb_diff <- (bbx-bbox) / (bbx[,2] - bbx[,1])
	inner.margins.new <- c(-bb_diff[2], -bb_diff[1], bb_diff[4], bb_diff[3])

	list(bbox=bbx, sasp=sasp, inner.margins=inner.margins.new)
}

split_raster <- function(r, f, drop=TRUE) {
	if (!is.factor(f)) {
		warning("f is not a factor", call. = FALSE)
		f <- as.factor(f)
	}
	bbx <- attr(r, "bbox")
	lev <- if (drop) {
		intersect(levels(f), f)	
	} else levels(f)
	lapply(lev, function(l){
		m <- matrix(as.numeric(!is.na(f) & f==l), ncol=r@ncols, nrow=r@nrows, byrow = TRUE)
		cls <- colSums(m)
		rws <- rev(rowSums(m))
		
		xrng <- range(which(cls!=0))
		yrng <- range(which(rws!=0))
		
		xrng[1] <- xrng[1] - 1
		yrng[1] <- yrng[1] - 1
		
		xlim <- xrng / r@ncols
		ylim <- yrng / r@nrows
		
		attr(r, "bbox") <- matrix(c(bbx[1,1] + (bbx[1,2] - bbx[1,1]) * xlim[1],
				 bbx[1,1] + (bbx[1,2] - bbx[1,1]) * xlim[2],
				 bbx[2,1] + (bbx[2,2] - bbx[2,1]) * ylim[1],
				 bbx[2,1] + (bbx[2,2] - bbx[2,1]) * ylim[2]), ncol=2, dimnames=dimnames(bbx), byrow = TRUE)
		r
	})
}
