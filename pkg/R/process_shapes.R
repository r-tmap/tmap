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
	shp.proj <- proj4string(shp)
	
	if (is.na(shp.proj)) {
		warning(paste("Currect projection of shape", shp_name, "unknown. Long-lat (WGS84) is assumed."))
		shp.proj <- "+proj=longlat +datum=WGS84"
		shp@proj4string <- CRS(shp.proj)
	}
	
	# edit and set projection
	isProjected <- !is.null(projection)
	if (isProjected) {
		projection <- get_proj4_code(projection)
		shp <- spTransform(shp, CRS(projection))
	} else {
		projection <- shp.proj
	}
	
	longlat <- !is.projected(shp)
	
	# define bounding box
	shp.bbox <- bbox(shp)
	
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
		}
		bbox <- matrix(c(xlim, ylim), ncol = 2, byrow=TRUE, 
					   dimnames=list(c("x", "y"), c("min", "max")))
	}
	

	bbox_asp <- get_bbox_asp(bbox, gm$inner.margins, longlat, pasp)
	bb <- bbox_asp$bbox
	sasp <- bbox_asp$sasp

	group_by <- gm$shp_nr != 0

	if (group_by) {
		x <- 1
		shp_by_name <- gm$shp_name
		if (gm$shp_nr != masterID) {
			shp_by <- shps[[gm$shp_nr]]
			proj_by <- proj4string(shp_by)
			if (is.na(proj_by)) {
				warning(paste("Currect projection of shape", shp_by_name, "unknown. Long-lat (WGS84) is assumed."))
				proj_by <- "+proj=longlat +datum=WGS84"
				shp_by@proj4string <- CRS(proj_by)
			}
			if (proj_by != projection) {
				shp_by <- spTransform(shp_by, CRS(projection))
			}
		} else {
			shp_by <- shp
		}
		
		nplots <- nlevels(data_by)
		shps_by <- split_shape(shp_by, f = data_by)
		bboxes <- lapply(shps_by, function(x){
			b <- x@bbox
			b[,1] <- pmax(b[,1], bb[,1])
			b[,2] <- pmin(b[,2], bb[,2])
			
			bsasp <- get_asp_ratio(b[1,], b[2,], longlat)
			
			if (bsasp > sasp) {
				yplus <- diff(b[2,]) * ((bsasp / sasp) - 1)
				b[2,] <- b[2,] + yplus * c(-.5, .5)
				if (b[2,2] > bb[2,2]) {
					b[2,] <- b[2,] - (b[2,2] - bb[2,2])
				} else if (b[2,1] < bb[2,1]) {
					b[2,] <- b[2,] + (bb[2,1] - b[2,1])
				}
			} else {
				xplus <- diff(b[1,]) * ((sasp / bsasp) - 1)
				b[1,] <- b[1,] + xplus * c(-.5, .5)
				if (b[1,2] > bb[1,2]) {
					b[1,] <- b[1,] - (b[1,2] - bb[1,2])
				} else if (b[1,1] < bb[1,1]) {
					b[1,] <- b[1,] + (bb[1,1] - b[1,1])
				}
			}
			
			get_bbox_asp(b, gm$inner.margins, longlat, pasp)$bbox
		})
	} else {
		shp_by_name <- ""
	}

	
	shps <- lapply(shps, function(x){
		shp_nm <- eval.parent(quote(names(X)))[substitute(x)[[3]]]
		
		if (shp_nm==shp_name) {
			x <- shp
		} else if (shp_nm==shp_by_name) {
			x <- shp_by
		} else {
			x.proj <- proj4string(x)
			if (is.na(x.proj)) {
				warning(paste("Currect projection of shape", shp_nm, "unknown. Long-lat (WGS84) is assumed."))
				x.proj <- "+proj=longlat +datum=WGS84"
				x@proj4string <- CRS(x.proj)
			}
			if (x.proj != projection) {
				x <- spTransform(x, CRS(projection))
			}
		}
		
		
		## try to crop the shape file at the bounding box in order to place bubbles and text labels inside the frame
		if (group_by) {
			lapply(bboxes, function(bb2){
				tryCatch({
					l <- length(x)
					crop_shape(x, bbox=bb2)
				}, error = function(e) {
					x@bbox <- bb2
					#cat("error\n")
					attr(x, "matchID") <- 1:length(x)
					x
				})
			})
		} else {
			tryCatch({
				l <- length(x)
				crop_shape(x, bbox=bb)
			}, error = function(e) {
				x@bbox <- bb
				#cat("error\n")
				attr(x, "matchID") <- 1:length(x)
				x
			})
		}
	})

	if (group_by) {
		shps <- lapply(1:nplots, function(i)lapply(shps, function(j)j[[i]]))
	}
	
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
	
	attr(shps, "sasp") <- ifelse(is.na(pasp), sasp, pasp)
	attr(shps, "dasp") <- dasp
	attr(shps, "legend_pos") <- legend_pos
	attr(shps, "group_by") <- group_by
	shps
}


get_bbox_asp <- function(bbox, inner.margins, longlat, pasp) {
	# extend bounding box for asp ratio
	bbrange <- bbox[,2] - bbox[,1]
	bbmarg <- inner.margins[c(2,1,4,3)]
	bbmarg[c(1,2)] <- -bbmarg[c(1,2)]
	bb <- bbox + rep(bbrange, 2) * bbmarg
	
	xlim <- bbox[1,]
	ylim <- bbox[2,]
	
	sasp <- get_asp_ratio(xlim, ylim, longlat)
	
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

get_asp_ratio <- function(xlim, ylim, longlat) {
	(diff(xlim)/diff(ylim)) * ifelse(longlat, cos((mean(ylim) * pi)/180), 1)
}
