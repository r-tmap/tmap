rasterCheckSize <- function(x, interactive) {
	# if (maxpixels < raster::ncell(x)) {
	# 	warning(paste("maximum number of pixels for Raster* viewing is",
	# 				  maxpixels, "; \nthe supplied Raster* has", ncell(x), "\n",
	# 				  "... decreasing Raster* resolution to", maxpixels, "pixels\n",
	# 				  "to view full resolution set 'maxpixels = ", ncell(x), "'"))

	tmapOptions <- get("tmapOptions", envir = .TMAP_CACHE)
	max.raster <- tmapOptions$max.raster
	show.messages <- tmapOptions$show.messages
	
	nc <- raster::ncell(x)
	mx <- max.raster[ifelse(interactive, "view", "plot")]
	if (nc > mx) {
		if (show.messages) message("Raster object has ", nc, " (", nrow(x), " by ", ncol(x), ") cells, which is larger than ",  mx, ", the maximum size determined by the option max.raster. Therefore, the raster will be shown at a decreased resolution of ", mx, " cells. Set tmap_options(max.raster = c(plot = ", nc, ", view = ", nc, ")) to show the whole raster.")
		if (nlayers(x) > 1) {
			x <- do.call(brick, lapply(1L:nlayers(x), function(i) {
				raster::sampleRegular(raster(x, layer = i), mx, asRaster = TRUE, useGDAL = TRUE)
			}))
		} else {
			x <- raster::sampleRegular(x, mx, asRaster = TRUE, useGDAL = TRUE)	
		}
	}
		
	
	return(x)
}

preprocess_shapes <- function(y, raster_facets_vars, gm, interactive) {
	shp <- y$shp
	
	show.messages <- get("tmapOptions", envir = .TMAP_CACHE)$show.messages

	
	if (is.null(shp)) return(list(shp=NULL, data=NULL, type="tiles"))

	shp.unit <- gm$shape.unit
	# shp.aa <- y[c("unit", "orig", "to", "total.area")]
	# names(shp.aa)[names(shp.aa)=="unit"] <- "target"
	# shp.aa <- shp.aa[!sapply(shp.aa, is.null)]
	
	shp.sim <- y[c("simplify", "keep.units", "keep.subunits", "method", "no_repair", "snap", "force_FC", "drop_null_geometries")]
	names(shp.sim)[names(shp.sim)=="simplify"] <- "fact"
	shp.sim <- shp.sim[!vapply(shp.sim, is.null, logical(1))]
	
	if (inherits(shp, c("stars", "Raster", "SpatialPixels", "SpatialGrid"))) {
		is.RGB <- attr(raster_facets_vars, "is.RGB") # true if tm_rgb is used (NA if qtm is used)
		rgb.vars <- attr(raster_facets_vars, "rgb.vars")
		to.Cat <- attr(raster_facets_vars, "to.Cat") # true if tm_raster(..., style = "cat) is specified
		max.value <- attr(raster_facets_vars, "max.value") # NULL is tm_raster is called, when tm_rgb is called: NA (default) when max color value is determined automatically.
		
		if (interactive) gm$shape.master_crs <- .crs_merc
		
		if (!inherits(shp, "stars")) shp <- stars::st_as_stars(shp)

		if (!has_raster(shp)) stop("object ", y$shp_name, " does not have a spatial raster", call. = FALSE)
		
		# attribute get from read_osm
		is.OSM <- attr(shp, "is.OSM")
		if (is.null(is.OSM)) is.OSM <- FALSE
		leaflet.server <- attr(shp, "leaflet.provider")
		if (is.null(leaflet.server)) leaflet.server <- NA
		
		
		if (is.na(is.RGB)) is.RGB <- FALSE # need for automatic check or change default?
		
		# get current projection (assume longlat if unkown)
		shp_crs <- sf::st_crs(shp)
		if (is.na(shp_crs)) {
			if (maybe_longlat(sf::st_bbox(shp))) {
				warning("Currect projection of shape ", y$shp_name, " unknown. Long lat (epsg 4326) coordinates assumed.", call. = FALSE)
				shp_crs <- .crs_longlat
				
				shp <- sf::st_set_crs(shp, shp_crs)
				shp <- stars::st_warp(shp, crs = shp_crs)	
			} else {
				stop("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
			}
		}

		# should raster shape be reprojected?
		if ((!interactive && !is.na(shp_crs) && !is.na(gm$shape.master_crs) && !identical(shp_crs$proj4string, gm$shape.master_crs$proj4string)) || (interactive && !sf::st_is_longlat(shp) && !st_is_merc(shp))) {
			if (is.na(gm$shape.master_crs)) stop("Master projection unknown, but needed to reproject raster shape.", call.=FALSE)
			shp <- sf::st_transform(shp, crs = gm$shape.master_crs)
			shp_bbox <- sf::st_bbox(shp)
			raster.projected <- TRUE
		} else {
			shp_bbox <- sf::st_bbox(shp)
			if (interactive && !st_is_merc(shp)) {
				shp <- stars::st_warp(cut_world_edges(shp), crs = .crs_merc)	
			}
			raster.projected <- FALSE
		}

		# set values in order to align data later on (with cropping)
		# shp2 <- setValues(shp2, values=1:ncell(shp2))

		# interactive raster require merc projection with longlat extent		
		# if (interactive) {
		# 	shp <- sf::st_transform(shp, crs = .crs_longlat)
		# 	raster.projected <- FALSE
		# 	# new_ext_ll <- extent(projectExtent(new_ext, crs = .crs_longlat$proj4string))
		# 	# shp2@extent <- new_ext_ll
		# 	# shp2@crs <- raster::crs(.crs_longlat$proj4string)
		# }
		
		

		
		shpnames <- stars::st_get_dimension_values(shp, "band")
		if (is.null(shpnames)) shpnames <- names(shp)
		
		data <- as.data.frame(matrix(shp[[1]], ncol = length(shpnames)))
		names(data) <- shpnames
		
		
		shp2 <- shp
		shp2[[1]] <- matrix(1L:(nrow(shp)*ncol(shp)), ncol = ncol(shp), nrow = nrow(shp))
		#shp2[[1]] <- t(matrix(1L:(nrow(shp)*ncol(shp)), ncol = nrow(shp), nrow = ncol(shp)))
		attr(shp2, "dimensions")$band <- NULL
		
		
		## to be consistent with Spatial objects:
		#attr(shp2, "bbox") <- bb(shp2)
		#attr(shp2, "proj4string") <-  #attr(shp2@crs, "projargs")

		attr(shp2, "bbox") <- shp_bbox #sf::st_bbox(shp2)
		
		#data <- shp#
		
		data$tmapfilter <- TRUE
		
		attr(data, "is.OSM") <- is.OSM
		attr(data, "leaflet.server") <- leaflet.server
		attr(data, "raster.projected") <- raster.projected
		
		#attr(data, "is.RGB") <- is.RGB
		
		type <- "raster"
		
	} else {
		# save_bbox (sp objects allow for custom bboxes, sf objects don't)
		shp_bbx <- sf::st_bbox(shp)
		
		kernel_density <- ("kernel_density" %in% names(attributes(shp)))
		isolines <- ("isolines" %in% names(attributes(shp)))
		
		if (y$check_shape) shp <- check_shape(shp, y$shp_name)

		## get data.frame from shapes, and store ID numbers in shape objects (needed for cropping)
		if (inherits(shp, "sfc")) {
			data <- data.frame(tmapID = seq_len(length(shp)))
			if (!is.null(names(shp))) names(shp) <- NULL
			shp <- st_sf(data, geometry=shp)
		} else {
			data <- shp
			st_geometry(data) <- NULL
			shp <- st_geometry(shp)
			if (!is.null(names(shp))) names(shp) <- NULL
			shp <- st_sf(tmapID = seq_len(length(shp)), geometry = shp)
		}
		
		data$tmapfilter <- if (is.null(y$filter)) rep(TRUE, nrow(shp)) else rep(y$filter, length.out = nrow(shp))
		
		# reproject if nessesary
		shp_crs <- sf::st_crs(shp)
		if (is.na(shp_crs)) {
			if (maybe_longlat(shp_bbx)) {
				warning("Currect projection of shape ", y$shp_name, " unknown. Long-lat (WGS84) is assumed.", call. = FALSE)
				shp_crs <- .crs_longlat
				shp <- sf::st_set_crs(shp, shp_crs)
			} else {
				stop("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
			}
		}
		if (!is.na(shp_crs) && !is.na(gm$shape.master_crs) && !identical(shp_crs$proj4string, gm$shape.master_crs$proj4string)) {
			shp2 <- sf::st_transform(shp, crs = gm$shape.master_crs)

			# override bounding box (since it now is projected)
			shp_bbx <- sf::st_bbox(shp2)
		} else {
			shp2 <- shp
		}
		
		if (inherits(st_geometry(shp2), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
			data$SHAPE_AREAS <- tmaptools::approx_areas(shp=shp2, target = paste(shp.unit, shp.unit, sep=" "))
			if (gm$shape.apply_map_coloring) attr(data, "NB") <- if (length(shp)==1) list(0) else get_neighbours(shp) #poly2nb(as(shp, "Spatial"))
			attr(data, "kernel_density") <- kernel_density
			type <- "polygons"
		} else if (inherits(st_geometry(shp2), c("sfc_LINESTRING", "sfc_MULTILINESTRING"))) {
			attr(data, "isolines") <- isolines
			## TODO update smooth_map to sf
			type <- "lines"
		} else if (inherits(st_geometry(shp2), c("sfc_POINT", "sfc_MULTIPOINT"))){
			type <- "points"
		} else {
			if (any(st_geometry_type(shp2) == "GEOMETRYCOLLECTION")) {
				gnew <- split_geometry_collection(st_geometry(shp2))
				ids <- attr(gnew, "ids")
				data <- data[ids, , drop = FALSE]
				shp2 <- st_sf(tmapID = 1L:nrow(data), geometry = gnew)
			}
			type <- "geometrycollection"
			attr(data, "kernel_density") <- FALSE
			attr(type, "types") <- get_types(st_geometry(shp2))
		}
		
		# simplify shape
		
		if (shp.sim$fact != 1 && type %in% c("polygons", "lines")) {
			## TODO convert fact to tolerance
			
			if (!requireNamespace("rmapshaper", quietly = TRUE)) {
				warning("rmapshaper package is needed to simplify the shape. Alternatively, st_simplify from the sf package can be used. See the underlying function tmaptools::simplify_shape for details.", call. = FALSE)
			} else {
				#shp2 <- st_simplify(shp2, preserveTopology = TRUE, dTolerance = shp.sim$fact)
				shp2 <- do.call(tmaptools::simplify_shape, c(list(shp=shp2), shp.sim))
				data <- data[shp2$tmapID, , drop=FALSE]
				shp2$tmapID <- seq_len(nrow(shp2))
			}
		}

		# be consistent with rasters (originated from sp objects)
		
		attr(shp2, "bbox") <- shp_bbx
		
		#attr(shp2, "bbox") <- shp_bbx
		#attr(shp2, "proj4string") <- st_crs(shp2)
		
		shpnames <- names(data)
		
	}
	
	point.per <- if (is.na(y$point.per)) ifelse(type %in% c("points", "geometrycollection"), "segment", "feature") else y$point.per

	attr(data, "shpnames") <- shpnames
	
	attr(shp2, "point.per") <- point.per
	attr(shp2, "line.center") <- y$line.center
	attr(shp2, "projected") <- !sf::st_is_longlat(shp2)
	list(shp=shp2, data=data, type=type)
}

get_types <- function(sfc) {
	tp <- st_geometry_type(sfc)
	types <- factor(rep(NA, length(sfc)), levels=c("polygons", "lines", "points", "collection"))
	types[tp %in% c("MULTIPOLYGON", "POLYGON")] <- "polygons"
	types[tp %in% c("MULTILINESTRING", "LINESTRING")] <- "lines"
	types[tp %in% c("MULTIPOINT", "POINT")] <- "points"
	types[tp == "GEOMETRYCOLLECTION"] <- "collection"
	if (any(is.na(types))) stop("The following geometry types are not supported: ", paste(unique(tp[is.na(types)]), collapse = ", "), call. = FALSE)
	types
}

split_geometry_collection <- function(sfc) {
	types <- get_types(sfc)
	res <- mapply(function(g, tp, id) {
		if (tp == "collection") {
			g2 <- suppressWarnings(list(st_collection_extract(g, "POLYGON"),
										st_collection_extract(g, "POINT"),
										st_collection_extract(g, "LINESTRING")))
			# tp2 <- factor(c("polygons", "points", "lines"), levels=c("polygons", "lines", "points"))
			g2 <- tryCatch({
				sel <- !vapply(g2, function(x)all(st_is_empty(x)), logical(1))
				g2[sel]
			}, error = function(e) {
				g2 <- lapply(g2, lwgeom::st_make_valid)
				sel <- !vapply(g2, function(x)all(st_is_empty(x)), logical(1))
				g2[sel]
			})

			# tp2 <- tp2[sel]
			#id2 <- rep(id, length(g2[[1]]))
			id2 <- rep(id, length(g2))
			list(g2, id2)
		} else {
			list(list(g), id)
		}
	}, sfc, types, 1:length(sfc), SIMPLIFY = FALSE)			
	#gnew <- st_sfc(do.call(st_sfc, lapply(lapply(res, "[[", 1), "[[", 1)), crs = st_crs(sfc))
	gnew <- st_sfc(do.call(st_sfc, do.call(c, lapply(res, "[[", 1))), crs = st_crs(sfc))
	ids <- do.call(c, lapply(res, "[[", 2))
	attr(gnew, "ids") <- ids
	gnew
}



