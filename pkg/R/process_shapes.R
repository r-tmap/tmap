process_shapes <- function(shps, g, gmeta, dw, dh) {
	gg <- gmeta$geo_grid
	gt <- gmeta$geo_theme
	
	sh <- (dh/gg$nrow) * (1-sum(gt$outer.margins[c(1,3)]))
	sw <- (dw/gg$ncol) * (1-sum(gt$outer.margins[c(2,4)]))
	
	dasp <- sw/sh
	pasp <- gt$asp
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
	projection <- g[[masterID]]$projection
	xlim <- g[[masterID]]$xlim
	ylim <- g[[masterID]]$ylim
	relative <- g[[masterID]]$relative
	bbox <- g[[masterID]]$bbox
	shp.proj <- proj4string(shp)
	
	if (is.na(shp.proj)) {
		warning(paste("Currect projection of shape", shp_names[masterID], "unknown. Long-lat (WGS84) is assumed."))
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
	
	# extend bounding box for asp ratio
	bb <- bbox
	bbrange <- bb[,2] - bb[,1]
	bbmarg <- gt$inner.margins[c(2,1,4,3)]
	bbmarg[c(1,2)] <- -bbmarg[c(1,2)]
	bb <- bb + rep(bbrange, 2) * bbmarg
	
	xlim <- bb[1,]
	ylim <- bb[2,]
	
	longlat <- !is.projected(shp)
	
	sasp <- if(longlat) {
		(diff(xlim)/diff(ylim)) * cos((mean(ylim) * pi)/180)
	} else {
		(diff(xlim)/diff(ylim))# * 2
	}
	
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
	
	shps <- lapply(shps, function(x){
		shp_name <- names(eval(sys.call(1)[[2]]))[substitute(x)[[3]]]
		x.proj <- proj4string(x)
		if (is.na(x.proj)) {
			warning(paste("Currect projection of shape", shp_name, "unknown. Long-lat (WGS84) is assumed."))
			x.proj <- "+proj=longlat +datum=WGS84"
			x@proj4string <- CRS(x.proj)
		}
		if (x.proj != projection) {
			x <- spTransform(x, CRS(projection))
		}
		
		## try to crop the shape file at the bounding box in order to place bubbles and text labels inside the frame
		x <- tryCatch({
			l <- length(x)
			crop_shape(x, bbox=bb)
		}, error = function(e) {
			x@bbox <- bb
			attr(x, "matchID") <- 1:length(x)
			x
		})
	})
	attr(shps, "sasp") <- ifelse(is.na(pasp), sasp, pasp)
	attr(shps, "dasp") <- dasp
	shps
}
