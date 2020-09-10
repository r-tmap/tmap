pre_process_shapes <- function(y, raster_facets_vars, gm, interactive) {
	shp <- y$shp
	tmapOptions = get("tmapOptions", envir = .TMAP_CACHE)
	
	show.messages <- tmapOptions$show.messages
	show.warnings <- tmapOptions$show.warnings
	
	
	if (is.null(shp)) return(list(shp=NULL, data=NULL, type="tiles"))

	shp.unit <- gm$shape.unit
	# shp.aa <- y[c("unit", "orig", "to", "total.area")]
	# names(shp.aa)[names(shp.aa)=="unit"] <- "target"
	# shp.aa <- shp.aa[!sapply(shp.aa, is.null)]
	
	shp.sim <- y[c("simplify", "keep.units", "keep.subunits", "method", "no_repair", "snap", "force_FC", "drop_null_geometries")]
	names(shp.sim)[names(shp.sim)=="simplify"] <- "fact"
	shp.sim <- shp.sim[!vapply(shp.sim, is.null, logical(1))]
	
	
	# process spatiotemporal array
	if (inherits(shp, c("sf", "sfc", "Spatial"))) {
		by_var = NULL
		treat_as_by = FALSE
	} else if (inherits(shp, "stars") && !has_raster(shp)) {
		# d = dim(shp)
		# v = lapply(1L:length(d), function(i) stars::st_get_dimension_values(shp, which = i))
		# is_sfc = vapply(v, inherits, logical(1), "sfc")
		# if (!any(is_sfc)) stop(y$shp_name, " is a stars object without raster nor geometry dimension", call. = FALSE)
		# vars = v[[which(!is_sfc)[1]]]
		# 

		by_var = names(shp)[1]
		
		if (length(shp) > 1L && show.warnings) {
			warning("Only the first attribute \"", by_var, "\" of ", y$shp_name, " will be plotted", call. = FALSE)
		}
		treat_as_by = TRUE
		
		shp = sf::st_as_sf(shp[by_var])
		shpnames = setdiff(names(shp), "geom")
	}
	
	
	if (inherits(shp, c("stars", "Raster", "SpatialPixels", "SpatialGrid"))) {
		is.RGB <- attr(raster_facets_vars, "is.RGB") # true if tm_rgb is used (NA if qtm is used)
		rgb.vars <- attr(raster_facets_vars, "rgb.vars")
		to.Cat <- attr(raster_facets_vars, "to.Cat") # true if tm_raster(..., style = "cat) is specified
		max.value <- attr(raster_facets_vars, "max.value") # NULL is tm_raster is called, when tm_rgb is called: NA (default) when max color value is determined automatically.
		
		if (interactive && is.numeric(tmapOptions$projection) && !identical(tmapOptions$projection, 0)) gm$shape.master_crs <- .crs_merc # leaflet excepts rasters in epsg 3857
		
		if (!inherits(shp, "stars")) shp <- stars::st_as_stars(shp)

		if (!has_raster(shp)) stop("object ", y$shp_name, " does not have a spatial raster", call. = FALSE)
		
		max.raster <- tmapOptions$max.raster[if(interactive) "view" else "plot"]

		
		dxy <- attr(st_dimensions(shp), "raster")$dimensions
		dvars <- setdiff(names(dim(shp)), dxy)
		
		bandnames <- stars::st_get_dimension_values(shp, dvars[1])
		treat_as_by <- !is.null(bandnames)
		
		if (treat_as_by) {
			by_var <- names(shp)[1]
			shpnames <- as.character(bandnames)
		} else {
			shpnames <- names(shp)
			by_var <- NULL
		}
		

		# estimate number of facets (for max.raster)
		# nfacets = if (identical(is.RGB, TRUE)) 1 else if (treat_as_by || is.na(raster_facets_vars[1])) length(shpnames) else length(na.omit(raster_facets_vars))
		# Not used yet, since it is hard to determine facets created with tm_facets(group = ...)
		if ((inherits(shp, "stars_proxy")) || y$raster.downsample) shp <- downsample_stars(shp, max.raster)
		
		
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
				if (show.warnings) warning("Currect projection of shape ", y$shp_name, " unknown. Long lat (epsg 4326) coordinates assumed.", call. = FALSE)
				shp_crs <- .crs_longlat
				
				#shp <- stars::st_warp(shp, crs = shp_crs)	
			} else {
				if (show.warnings) warning("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
				shp_crs <- st_crs("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0")
			}
			shp <- sf::st_set_crs(shp, shp_crs)
		}

		# # should raster shape be reprojected?
		if (interactive && !gm$shape.crsSimple) {
			if (sf::st_is_longlat(shp_crs)) {
				shp_bbox <- sf::st_bbox(shp)
				shp <- transwarp(shp, crs = .crs_merc, y$raster.warp)
			} else  if (st_is_merc(shp_crs)) {
				shp_bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(shp)), crs = .crs_longlat))
			} else {
				shp_bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(shp)), crs = .crs_longlat))
				shp <- transwarp(shp, crs = gm$shape.master_crs, y$raster.warp)
			}
		} else {
			if (interactive && show.warnings) warning("It is not possible yet to visualize raster objects (in this case ", y$shp_name, ") interactively in an unprojected crs (projection = 0)", call. = FALSE)
			
			if (!identical(shp_crs$proj4string, gm$shape.master_crs$proj4string)) {
				shp <- transwarp(shp, crs = gm$shape.master_crs, y$raster.warp)
			}
			shp_bbox <- sf::st_bbox(shp)
		}
		
		
		if (length(shp) == 1) {
			# one attribute (may contain multiple bands/times)
			isF <- is.factor(shp[[1]])
			if (isF) {
				lvls <- levels(shp[[1]])
				
				clrs = attr(shp[[1]], "colors")
				if (!is.null(clrs)) clrs = rep(clrs, length.out = length(lvls))
				
				m <- matrix(as.integer(shp[[1]]), ncol = length(shpnames))	
				if (!length(lvls)) isF <- FALSE
			} else {
				m <- matrix(shp[[1]], ncol = length(shpnames))
			}
			data <- as.data.frame(m)
			if (isF) {
				data <- lapply(data, structure, class = "factor", levels = lvls, clrs = clrs)
				class(data) <- "data.frame"
				attr(data, "row.names") <- .set_row_names(length(data[[1]]))
				#attr(data, "clrs") <- clrs
			}
			names(data) <- shpnames
		} else {
			# multiple attributes (assumed: one band)
			data <- as.data.frame(shp)[shpnames]
		}
		isnum <- sapply(data, is.numeric)
		
		if (is.na(is.RGB)) {
			# check for qtm (where is.RGB = NA)
			if (!all(isnum) || ncol(m) != 3L) {
				is.RGB <- FALSE
			} else {
				m <- as.matrix(data)
				mxdata <- max(m, na.rm = TRUE)
				mndata <- min(m, na.rm = TRUE)
				
				if (mndata > 0 && mxdata < max.value) {
					if (mxdata <= 1) {
						max.value <- 1
						message("Numeric values of ", y$shp_name, " interpreted as RGB values with max.value = 1. Run tm_shape(", y$shp_name, ") + tm_raster() to visualize the data.")
					} else {
						message("Numeric values of ", y$shp_name, " interpreted as RGB values with max.value = 255. Run tm_shape(", y$shp_name, ") + tm_raster() to visualize the data.")
					}
					is.RGB <- TRUE
					rgb.vars <- 1:3
				} else {
					is.RGB <- FALSE
				}
			}
		} else if (is.RGB) {
			if (!all(isnum)) stop("Raster data is not numeric", call. = FALSE)

			m <- as.matrix(data)
			mxdata <- max(m, na.rm = TRUE)
			mndata <- min(m, na.rm = TRUE)
			
			if (!all(rgb.vars %in% 1:ncol(data))) stop("Specified rgb(a) bands are ", paste(rgb.vars, collapse = ", "), " whereas the number of layers is ", ncol(data), call. = FALSE)
			
			if  (!(mndata>=0 || mxdata <= max.value)) {
				m[m < 0] <- 0
				m[m > max.value] <- max.value
				if (show.warnings) warning("Raster values found that are outside the range [0, ", max.value, "]", call. = FALSE)
				data <- as.data.frame(m)
			}
			if (mxdata <= 1 && max.value == 255) message("No values higher than 1 found. Probably, max.value should be set to 1.")
		}
		
		if (is.RGB) {
			#layerIDs <- rgb.vars
			#data <-data[,rgb.vars]
			
			data <- data.frame(PIXEL__COLOR = raster_colors(as.matrix(data[,rgb.vars]), use.colortable = FALSE, max.value = max.value))
		} else {
			if (is.na(raster_facets_vars[1]) || treat_as_by) {
				raster_facets_vars <- shpnames
			} else {
				raster_facets_vars <- na.omit(raster_facets_vars)
			}
			layerIDs <- match(raster_facets_vars, shpnames)
			layerIDs <- na.omit(layerIDs)
			if (length(layerIDs) == 0L) layerIDs <- 1
			data <- data[, layerIDs, drop = FALSE]
		}
		

		# flatten
		
		shp2 <- shp[1]
		shp2[[1]] <- matrix(1L:(nrow(shp)*ncol(shp)), ncol = ncol(shp), nrow = nrow(shp))
		attr(shp2, "dimensions") <- attr(shp2, "dimensions")[dxy]
		
		## to be consistent with Spatial objects:
		#attr(shp2, "bbox") <- bb(shp2)
		#attr(shp2, "proj4string") <-  #attr(shp2@crs, "projargs")

		attr(shp2, "bbox") <- shp_bbox #sf::st_bbox(shp2)
		
		#data <- shp#
		
		data$tmapfilter <- TRUE
		
		attr(data, "is.OSM") <- is.OSM
		attr(data, "leaflet.server") <- leaflet.server
		#attr(data, "raster.projected") <- raster.projected
		
		attr(data, "is.RGB") <- is.RGB
		#attr(data, "treat_as_by") <- treat_as_by
		#attr(data, "by_var") <- by_var
		
		
		type <- "raster"
		
	} else {
		# save_bbox (sp objects allow for custom bboxes, sf objects don't)
		
		
		kernel_density <- ("kernel_density" %in% names(attributes(shp)))
		isolines <- ("isolines" %in% names(attributes(shp)))
		
		if (y$check_shape) shp <- pre_check_shape(shp, y$shp_name, show.warnings)

		shp_bbx <- sf::st_bbox(shp)
		
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
		
		# check crs of shp (master shape crs is already checked in print_tmap)
		shp_crs <- sf::st_crs(shp)
		if (is.na(shp_crs)) {
			if (maybe_longlat(shp_bbx)) {
				if (show.warnings) warning("Currect projection of shape ", y$shp_name, " unknown. Long-lat (WGS84) is assumed.", call. = FALSE)
				shp_crs <- .crs_longlat
				
			} else {
				if (show.warnings) warning("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
				shp_crs <- st_crs("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0")
			}
			shp <- sf::st_set_crs(shp, shp_crs)
			
		}

		# reproject if nessesary
		if (shp_crs != gm$shape.master_crs) {
			shp2 <- sf::st_transform(shp, crs = gm$shape.master_crs)

			# override bounding box (since it now is projected)
			shp_bbx <- sf::st_bbox(shp2)
		} else {
			shp2 <- shp
		}
		
		if (inherits(st_geometry(shp2), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
			data$SHAPE_AREAS <- tmaptools::approx_areas(shp=shp2, target = paste(shp.unit, shp.unit, sep=" "))
			
			zeros = as.numeric(data$SHAPE_AREAS)==0
			
			if (all(zeros)) {
				stop("All polygons of ", y$shp_name, " have an area of 0, possibly caused by transformation.", call. = FALSE)
			} else if (any(zeros)) {
				sel = as.numeric(data$SHAPE_AREAS)!=0
				data = data[sel, , drop = FALSE]
				shp2 = shp2[sel, ]
				
				data$tmapID = 1L:nrow(data)
				shp2$tmapID = 1L:nrow(data)
			}
			
			
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
				if (show.warnings) warning("rmapshaper package is needed to simplify the shape. Alternatively, st_simplify from the sf package can be used. See the underlying function tmaptools::simplify_shape for details.", call. = FALSE)
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

		if (!treat_as_by) shpnames <- names(data)
	}
	
	point.per <- if (is.na(y$point.per)) ifelse(type %in% c("points", "geometrycollection"), "segment", "feature") else y$point.per

	attr(data, "shpnames") <- shpnames
	attr(data, "treat_as_by") <- treat_as_by
	attr(data, "by_var") <- by_var
	
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
				g2 <- lapply(g2, sf::st_make_valid)
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



