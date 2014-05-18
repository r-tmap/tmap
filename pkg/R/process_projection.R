process_projection <- function(shps, g) {
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
	
	# edit and set projection
	isProjected <- !is.null(projection)
	
	if (isProjected) {
		projection <- get_proj4_code(projection)
		if (is.na(shp.proj)) {
			warning("Currect projection of shape object unknown. Long-lat (WGS84) is assumed.")
			shp@proj4string <- CRS("+proj=longlat +datum=WGS84")
		}
		shp <- spTransform(shp, CRS(projection))
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
		shp@bbox <- bbox
	}

	shps[[masterID]] <- shp
	
	if (isProjected) {
		shps[-masterID] <- lapply(shps[-masterID], function(x) {
			if (is.na(shp.proj)) {
				x@proj4string <- CRS("+proj=longlat +datum=WGS84")
			}
			x <- spTransform(x, CRS(projection))
			x@bbox <- bbox
			x
		})
	} else {
		# for consistency, use first projection on other shapes
		if (!is.na(shp.proj)) {
			shps[-masterID] <- lapply(shps[-masterID], function(x) {
				shpx.proj <- proj4string(shp)
				if (is.na(shpx.proj) || shpx.proj!=shp.proj) {
					x <- spTransform(x, CRS(shp.proj))
				}
				x@bbox <- bbox
				x
			})	
		}
	}
	
	shps
}
