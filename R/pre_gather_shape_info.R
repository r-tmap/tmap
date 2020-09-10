pre_gather_shape_info <- function(x, interactive) {
	tmapOptions = get("tmapOptions", envir = .TMAP_CACHE)
	show.warnings <- tmapOptions$show.warnings
	
	## identify shape blocks
	shape.id <- which(names(x)=="tm_shape")
	nshps <- length(shape.id)
	if (!nshps) stop("Required tm_shape layer missing.", call. = FALSE)
	
	## find "MAP_COLORING" values
	apply_map_coloring <- if ("tm_fill" %in% names(x)) {
		any(vapply(x[which(names(x)=="tm_fill")], function(i)identical(i$col[1],"MAP_COLORS"), logical(1)))
	} else FALSE
	
	## find master shape
	is_raster <- vapply(x[shape.id], function(xs) {
		!is.null(xs$shp) && inherits(xs$shp, c("stars", "Raster", "SpatialPixels", "SpatialGrid"))
	}, logical(1))
	is_master <- vapply(x[shape.id], "[[", logical(1), "is.master")
	#	any_raster <- any(is_raster)
	masterID <- if (!length(which(is_master))) {
		which(is.na(is_master))[1]
	} else which(is_master)[1]
	is_raster_master <- is_raster[masterID]
	
	## find master projection (and set to longlat when in view mode)
	master_crs <- sf::st_crs(x[[shape.id[masterID]]]$projection)
	mshp_raw <- x[[shape.id[masterID]]]$shp
	if (!inherits(mshp_raw, c("stars", "Raster", "sf", "sfc", "Spatial"))) stop("Object ", x[[shape.id[masterID]]]$shp_name, " is neither from class sf, stars, Spatial, nor Raster.", call. = FALSE) # shapes are later checked in pre_check_shape
	
	mshp_crs <- sf::st_crs(mshp_raw)
	bbx_raw <- sf::st_bbox(mshp_raw)
	
	# Checks whether master shape has no crs and has coordinates outside -180-180, -90-90. The crss is futher checked in preprocess_shapes 
	if (is.na(mshp_crs)) {
		if (maybe_longlat(bbx_raw)) {
			mshp_crs <- .crs_longlat
		} else {
			if (show.warnings) warning("The projection of the shape object ", x[[shape.id[masterID]]]$shp_name, " is not known, while it seems to be projected.", call.=FALSE)
			mshp_crs <- st_crs("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0")
		}
	}
	
	if (is.na(master_crs)) master_crs <- mshp_crs
	orig_crs <- master_crs # needed for adjusting bbox in process_shapes
	
	
	if (interactive) {
		# Set master projection to 4326 if projection != 0 (L.CRS.Simple)
		# Find out whether projection == 0:
		option_prj = get("tmapOptions", envir = .TMAP_CACHE)$projection
		crsSimple <- inherits(option_prj, "leaflet_crs") || option_prj == 0
		if (any(names(x)=="tm_layout")) {
			crsSimples <- vapply(x[which(names(x)=="tm_layout")], function(xi) {
				if (any(names(xi) == "projection")) {
					xi$projection == 0
				} else {
					as.logical(NA)
				}
			}, FUN.VALUE = logical(1))
			if (!all(is.na(crsSimples))) {
				crsSimple <- tail(na.omit(crsSimples), 1)
			}
		}
		if (!crsSimple) master_crs <- .crs_longlat
	} else {
		crsSimple = TRUE
	}
	
	## get raster and group by variable name (needed for eventual reprojection of raster shapes)
	raster_facets_vars <- lapply(1:nshps, function(i) {
		from <- shape.id[i] + 1
		to <- ifelse(i==nshps, length(x), shape.id[i+1]-1)
		fid <- which(names(x)[from:to]=="tm_facets")
		rid <- which(names(x)[from:to]=="tm_raster")
		
		if (length(rid)) {
			max.value <- x[[from-1+rid[1]]]$max.value
			is.RGB <- x[[from-1+rid[1]]]$is.RGB
			rgb.vars <- x[[from-1+rid[1]]]$rgb.vars
			to.Cat <- x[[from-1+rid[1]]]$style == "cat"
		} else {
			max.value <- NA
			is.RGB <- FALSE
			rgb.vars <- NULL
			to.Cat <- FALSE
		}
		
		res <- c(if (length(fid)) x[[from-1+fid[1]]]$by else NULL,
				 if (length(rid)) x[[from-1+rid[1]]]$col else NULL)
		if (is.null(res)) res <- NA
		attr(res, "max.value") <- max.value
		attr(res, "is.RGB") <- is.RGB
		attr(res, "rgb.vars") <- rgb.vars
		attr(res, "to.Cat") <- to.Cat
		res
	})
	
	## get arguments related to units (approx_areas)
	unit <- x[[shape.id[masterID]]]$unit
	if (is.null(unit)) unit <- get("tmapOptions", envir = .TMAP_CACHE)$unit
	if (unit == "metric") unit <- "km"
	if (unit == "imperial") unit <- "mi"
	
	# units_args <- x[[shape.id[masterID]]][c("unit", "orig", "to", "total.area")]
	# names(units_args)[names(units_args)=="unit"] <- "target"
	# units_args <- units_args[!sapply(units_args, is.null)]
	
	## get arguments related to bb
	bb_args <- x[[shape.id[masterID]]][intersect(names(x[[shape.id[masterID]]]), c("ext", "cx", "cy", "width", "height", "xlim", "ylim", "relative", "asp.limit"))]
	bb_args$x <- x[[shape.id[masterID]]]$bbox
	
	## add other shape arguments
	# point.per <- x[[shape.id[masterID]]]$point.per
	# line.center <- x[[shape.id[masterID]]]$line.center
	
	list(shape.id=shape.id,
		 shape.nshps=nshps,
		 shape.apply_map_coloring=apply_map_coloring,
		 shape.is_raster_master=is_raster_master,
		 shape.masterID=masterID,
		 shape.master_crs=master_crs,
		 shape.crsSimple = crsSimple,
		 shape.orig_crs=orig_crs,
		 shape.bbx_raw=bbx_raw,
		 shape.unit=unit,
		 shape.bb_args=bb_args,
		 # shape.point.per=point.per,
		 # shape.line.center=line.center,
		 shape.raster_facets_vars=raster_facets_vars)
}
