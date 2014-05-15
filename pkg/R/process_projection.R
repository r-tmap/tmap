process_projection <- function(g) {
	# find master
	masterID <- which(sapply(g, function(x)!is.null(x$projection) || !is.null(x$xlim) || !is.null(x$ylim) || !is.null(x$bbox)))
	
	if (length(masterID)>1) {
		warning("Multiple projections or bounding boxes defined. First one is taken.")
		masterID <- masterID[1]
	}
	if (!length(masterID)) masterID <- 1
	
	# get master shape and info
	shp <- g[[masterID]]$shp
	projection <- g[[masterID]]$projection
	xlim <- g[[masterID]]$xlim
	ylim <- g[[masterID]]$ylim
	relative <- g[[masterID]]$relative
	bbox <- g[[masterID]]$bbox
	shp.proj <- proj4string(shp)
	
	# edit and set projection
	if (!is.null(projection)) {
		projection <- get_proj4_code(projection)
		if (is.na(shp.proj)) {
			warning("Currect projection of shape object unknown. Long-lat (WGS84) is assumed.")
			shp@proj4string <- CRS("+proj=longlat +datum=WGS84")
		}
		shp <- spTransform(shp, CRS(projection))
		g[[masterID]]$shp <- shp
		g[-masterID] <- lapply(g[-masterID], function(x) {
			if (is.na(shp.proj)) {
				x$shp@proj4string <- CRS("+proj=longlat +datum=WGS84")
			}
			x$shp <- spTransform(x$shp, CRS(projection))
			x
		})
	} else {
		# for consistency, use first projection on other shapes
		if (!is.na(shp.proj)) {
			g[-masterID] <- lapply(g[-masterID], function(x) {
				shpx.proj <- proj4string(x$shp)
				if (is.na(shpx.proj) || shpx.proj!=shp.proj) {
					x$shp <- spTransform(x$shp, CRS(shp.proj))
				}
				x
			})	
		}
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
	
	## set bounding box
	g <- lapply(g, function(x){
		x$shp@bbox <- bbox
		x
	})
	
	g
}
