preprocess_shapes <- function(y, apply_map_coloring, master_proj, master_bbx, interactive, raster_facets_vars) {
	shp <- y$shp
	shp.unit <- y$unit
	shp.unit.size <- y$unit.size
	
	if (inherits(shp, c("Raster", "SpatialPixels", "SpatialGrid"))) {
		if (interactive) master_proj <- get_proj4("merc")
		if (inherits(shp, "Spatial")) {
			
			# attribute get from read_osm
			is.OSM <- attr(shp, "is.OSM")
			
			if (!("data" %in% slotNames(shp))) stop("No data found in raster shape. Please specify a SpatialGridDataFrame or Raster shape object.")

			if (is.na(raster_facets_vars[1]) || !any(raster_facets_vars %in% names(shp))) {
				is.RGB <- (ncol(shp)==3 && all(vapply(shp@data, FUN = function(x) {
					if (!is.numeric(x)) {
						FALSE
					} else {
						min(x)>=0 && max(x<=255)
					}
				}, FUN.VALUE = logical(1))))
				if (is.RGB) shp@data <- raster_colors(shp)
				raster_facets_vars <- names(shp)[1]
			} else raster_facets_vars <- intersect(raster_facets_vars, names(shp))
			
			## subset data, make factors of non-numeric variables
			#raster_data <- preprocess_raster_data(shp@data, raster_facets_vars)
			
			lvls <- get_data_frame_levels(shp@data[, raster_facets_vars, drop=FALSE])
			
			## use bilinear interpolation for numeric data only
			use_interp <- (all(sapply(lvls, is.null)))
			shp <- raster::subset(brick(shp), raster_facets_vars, drop=FALSE)
		} else {
			is.OSM <- FALSE
			
			# color values are encoded by a colortable (and not interpreted as factors)
			if (length(colortable(shp))>0) {
				lvls <- list(colortable(shp))
				if (nlayers(shp)>1) shp <- raster::subset(shp, 1)
				shp <- setValues(shp, getValues(shp) + 1L)
				names(shp) <- "PIXEL__COLOR"
				use_interp <- FALSE
			} else {
				# in order to not loose factor levels, subset the data here
				shpnames <- get_raster_names(shp)
				if (is.na(raster_facets_vars[1]) || !any(raster_facets_vars %in% names(shp))) {
					is.RGB <- (nlayers(shp)>=3 && nlayers(shp)<=4 && minValue(shp)>=0 && maxValue(shp)<= 255)
					if (is.RGB) {
						pix <- raster_colors(shp)$PIXEL__COLOR
						shp <- raster(shp, layer=0)
						shp <- setValues(shp, as.integer(pix))
						names(shp) <- "PIXEL__COLOR"
						raster_facets_vars <- "PIXEL__COLOR"
						lvls <- list(levels(pix))
					} else raster_facets_vars <- shpnames[1]
				} else {
					is.RGB <- FALSE
					raster_facets_vars <- intersect(raster_facets_vars, shpnames)
				}
				
				if (!is.RGB) {
					layerIDs <- match(raster_facets_vars, shpnames)
					lvls <- get_raster_levels(shp, layerIDs)
					
					#raster_data <- get_raster_data(shp)[, raster_facets_vars, drop=FALSE]
					#lvls <- get_data_frame_levels(raster_data)
					# subset raster to get rid of non-used variables (to make projectRaster faster)
					if (nlayers(shp)>1) shp <- raster::subset(shp, raster_facets_vars)
					
					#lvls <- get_raster_levels(shp)
					use_interp <- (all(sapply(lvls, is.null)))
					
					
				} else {
					use_interp <- FALSE
				}
			}
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
		if ((!is.na(shp_proj) && !is.na(master_proj) && shp_proj!=master_proj) || interactive) {
			if (is.na(master_proj)) stop("Master projection unknown, but needed to reproject raster shape.", call.=FALSE)
			new_ext <- tryCatch({
			  	suppressWarnings(projectExtent(shp, crs = CRS(get_proj4(master_proj))))
			}, error=function(e){
		  		NULL
		  	})
			
			if (is.null(new_ext)) {
				shp <- crop_shape(shp, bb(master_bbx, projection = shp_proj, current.projection = master_proj))	
				new_ext <- tryCatch({
					suppressWarnings(projectExtent(shp, crs = CRS(get_proj4(master_proj))))
				}, error=function(e){
					stop("Unable to reproject raster shape \"", y$shp_name, "\", probably due to non-finite points.", call. = FALSE)
				})
			}
		} else new_ext <- NULL

		if (!is.null(new_ext)) {
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
					d2 <- as.integer(d)+1L
				} else {
					plusone <- min(d, na.rm=TRUE)==0
					if (!is.integer(d)) {
						d2 <- as.integer(d) + plusone
					} else {
						d2 <- d+plusone
					}
				}
				levels(d2) <- l
				class(d2) <- "factor"
				d2
			} else d
		}, data, lvls, SIMPLIFY=FALSE))

		# set values in order to align data later on (with cropping)
		shp2 <- setValues(shp2, values=1:ncell(shp2))

		# interactive raster require merc projection with longlat extent		
		if (interactive) {
			new_ext_ll <- extent(projectExtent(new_ext, crs = CRS(get_proj4("longlat"))))
			shp2@extent <- new_ext_ll
			shp2@crs <- CRS(get_proj4("longlat"))
		}
		
		## to be consistent with Spatial objects:
		attr(shp2, "bbox") <- bbox(shp2)
		attr(shp2, "proj4string") <- shp2@crs
		
		attr(data, "is.OSM") <- is.OSM
		
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
			attr(data, "kernel_density") <- ("kernel_density" %in% names(attributes(shp)))
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