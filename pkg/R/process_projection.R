process_projection <- function(shps, g) {
	nx <- length(shps)
	shp_names <- sapply(g, function(i)i[[1]])
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

	mapply(FUN=function(x, i){
		if (i==masterID) {
			shp
		} else {
			x.proj <- proj4string(x)
			if (is.na(x.proj)) {
				warning(paste("Currect projection of shape", shp_names[i], "unknown. Long-lat (WGS84) is assumed."))
				x.proj <- "+proj=longlat +datum=WGS84"
				x@proj4string <- CRS(x.proj)
			}
			if (x.proj != shp.proj) {
				x <- spTransform(x, CRS(projection))
			}
			x@bbox <- bbox
			x
		}
	}, shps, 1:nx, SIMPLIFY=FALSE)
}
