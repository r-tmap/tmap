preprocess_shapes <- function(y, apply_map_coloring, master_proj, interactive, raster_facets_vars) {
	shp <- y$shp
	shp.unit <- y$unit
	shp.unit.size <- y$unit.size
	
	if (inherits(shp, c("Raster", "SpatialPixels", "SpatialGrid"))) {
		if (interactive) master_proj <- get_proj4("merc")
		if (inherits(shp, "Spatial")) {
			# attribute get from read_osm
			is.OSM <- attr(shp, "is.OpenStreetMap")
			
			if (!("data" %in% slotNames(shp))) {
				stop("No data found in raster shape. Please specify a SpatialGridDataFrame or Raster shape object.")
			}

			if (is.na(raster_facets_vars[1]) || !any(raster_facets_vars %in% names(shp))) raster_facets_vars <- names(shp)[1]
			raster_facets_vars <- intersect(raster_facets_vars, names(shp))
			
			## subset data, make factors of non-numeric variables
			raster_data <- preprocess_raster_data(shp@data, raster_facets_vars)

			## color values are interpreted as factors and need to be cast back to characters
			#is_color <- attr(shp@data, "is_color")
			has_color_table <- FALSE

			## use bilinear interpolation for numeric data only
			use_interp <- (all(sapply(shp@data, is.numeric)))
			shp <- raster::subset(brick(shp), raster_facets_vars, drop=FALSE)
		} else {
			is.OSM <- FALSE
			
			# color values are encoded by a colortable (and not interpreted as factors)
			has_color_table <- (length(colortable(shp))>0)
			#is_color <- rep(FALSE, length(y$col))
			
			# in order to not loose factor levels, subset the data here
			if (is.na(raster_facets_vars[1]) || !any(raster_facets_vars %in% names(shp))) raster_facets_vars <- names(shp)[1]
			raster_facets_vars <- intersect(raster_facets_vars, names(shp))
			raster_data <- get_raster_data(shp)[, raster_facets_vars, drop=FALSE]
			
			is_num <- sapply(raster_data, is.numeric)
			
			# subset raster to get rid of non-used variables (to make projectRaster faster)
			if (nlayers(shp)>1) shp <- raster::subset(shp, raster_facets_vars)
			use_interp <- all(is_num) && !has_color_table
		}
		
		# get factor levels (to be restored later)
		lvls <- lapply(raster_data, levels)
		# override factor levels with colortable values
		if (has_color_table) {
			lvls <- lapply(lvls, function(l) colortable(shp))
		}
		
		# get current projection (assume longlat if unkown)
		shp_proj <- get_projection(shp)
		if (is.na(shp_proj)) {
			if (!is_projected(shp)) {
				warning("Currect projection of shape ", y$shp_name, " unknown. Long-lat (WGS84) is assumed.", call. = FALSE)
				shp_proj <- get_proj4("longlat")
				shp <- set_projection(shp, current.projection = shp_proj)
			} else {
				warning("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
			}
		}
		
		# should raster shape be reprojected?
		
		new_ext <- suppressWarnings(projectExtent(shp, crs = CRS(get_proj4(master_proj))))
		if (!is.na(shp_proj) && !is.na(master_proj) && shp_proj!=master_proj) {
			shpTmp <- suppressWarnings(projectRaster(shp, to=new_ext, crs=CRS(master_proj), method = ifelse(use_interp, "bilinear", "ngb")))
			shp2 <- raster(shpTmp)
			data <- get_raster_data(shpTmp)
		} else {
			shp2 <- raster(shp)
			data <- get_raster_data(shp)
		}
		
		# restore factor levels
		data <- as.data.frame(mapply(function(d, l) {
			if (!is.null(l) && !is.factor(d)) {
				if (is.logical(d)) {
					factor(as.integer(d)+1L, levels=1L:length(l), labels=l)	
				} else {
					plusone <- min(d, na.rm=TRUE)==0
					factor(d+plusone, levels=1L:length(l), labels=l)
				}
			} else d
		}, data, lvls, SIMPLIFY=FALSE))

		# set values in order to align data later on (with cropping)
		shp2 <- setValues(shp2, values=1:ncell(shp2))

		# cast color values back to characters
# 		if (any(is_color)) {
# 			data[, is_color] <- lapply(data[, is_color], as.character)
# 		}
		
		#apply color table
		if (has_color_table) {
			data <- data.frame(PIXEL__COLOR=data[[1]])
			levels(data$PIXEL__COLOR) <- colortable(shp)
			class(data$PIXEL__COLOR) <- "factor"
		}

		# interactive raster require merc projection with longlat extent		
		if (interactive) {
			new_ext_ll <- extent(projectExtent(new_ext, crs = CRS(get_proj4("longlat"))))
			shp2@extent <- new_ext_ll
			shp2@crs <- CRS(get_proj4("longlat"))
		}
		
		## to be consistent with Spatial objects:
		attr(shp2, "bbox") <- bbox(shp2)
		attr(shp2, "proj4string") <- shp2@crs
		
		attr(data, "is.OpenStreetMap") <- is.OSM
		
		type <- "raster"
		
	} else {
		if (!inherits(shp, "Spatial")) {
			stop("Object ", y$shp_name, " is neither from class Spatial nor Raster.", call. = FALSE)
		}
		
		## get data.frame from shapes, and store ID numbers in shape objects (needed for cropping)
		newData <- data.frame(tmapID = seq_len(length(shp)))
		if ("data" %in% slotNames(shp)) {
			data <- shp@data
			shp@data <- newData
		} else {
			data <- newData
			shp <- if (inherits(shp, "SpatialPolygons")) {
				SpatialPolygonsDataFrame(shp, data = newData, match.ID = FALSE)
			} else if (inherits(shp, "SpatialLines")) {
				SpatialLinesDataFrame(shp, data = newData, match.ID = FALSE)
			} else if (inherits(shp, "SpatialPoints")) {
				SpatialPointsDataFrame(shp, data = newData, match.ID = FALSE)
			}
		}
		
		# reproject if nessesary
		shp_proj <- get_projection(shp)
		if (is.na(shp_proj)) {
			if (!is_projected(shp)) {
				warning("Currect projection of shape ", y$shp_name, " unknown. Long-lat (WGS84) is assumed.", call. = FALSE)
				shp_proj <- get_proj4("longlat")
				shp <- set_projection(shp, current.projection = shp_proj)
			} else {
				warning("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
			}
		}
		if (!is.na(shp_proj) && !is.na(master_proj) && shp_proj!=master_proj) {
			shp2 <- spTransform(shp, CRS(master_proj))
		} else {
			shp2 <- shp
		}
		
		if (inherits(shp2, "SpatialPolygonsDataFrame")) {
			data$SHAPE_AREAS <- approx_areas(shp2, unit=shp.unit, unit.size = shp.unit.size)
			attr(data, "AREAS_is_projected") <- is_projected(shp2)
			if (apply_map_coloring) attr(data, "NB") <- if (length(shp)==1) list(0) else poly2nb(shp)
			attr(data, "dasymetric") <- ("dasymetric" %in% names(attributes(shp)))
			type <- "polygons"
		} else if (inherits(shp2, "SpatialLinesDataFrame")) {
			attr(data, "isolines") <- ("isolines" %in% names(attributes(shp)))
			type <- "lines"
		} else if (inherits(shp2, "SpatialPointsDataFrame")) {
			type <- "points"
		}
	}
	attr(shp2, "projected") <- is_projected(shp2)
	
	list(shp=shp2, data=data, type=type)
}