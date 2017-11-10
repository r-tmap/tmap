preprocess_shapes <- function(y, raster_facets_vars, gm, interactive) {
	shp <- y$shp
	shp.aa <- y[c("unit", "orig", "to", "total.area")]
	names(shp.aa)[names(shp.aa)=="unit"] <- "target"
	shp.aa <- shp.aa[!sapply(shp.aa, is.null)]
	
	shp.sim <- y[c("simplify", "keep.units", "keep.subunits", "method", "no_repair", "snap", "force_FC", "drop_null_geometries")]
	names(shp.sim)[names(shp.sim)=="simplify"] <- "fact"
	shp.sim <- shp.sim[!sapply(shp.sim, is.null)]
	
	
	if (inherits(shp, c("Raster", "SpatialPixels", "SpatialGrid"))) {
		is.RGB <- attr(raster_facets_vars, "is.RGB")
		to.Cat <- attr(raster_facets_vars, "to.Cat")
		if (interactive) gm$shape.master_CRS <- .CRS_merc
		if (inherits(shp, "Spatial")) {
			
			# attribute get from read_osm
			is.OSM <- attr(shp, "is.OSM")
			leaflet.provider <- attr(shp, "leaflet.provider")

			
			if (!("data" %in% slotNames(shp))) stop("No data found in raster shape. Please specify a SpatialGridDataFrame or Raster shape object.")

			if (is.na(raster_facets_vars[1]) || !any(raster_facets_vars %in% names(shp))) {
				convert.RGB <- if (!identical(is.RGB, FALSE)) {
					(ncol(shp)>=3 && ncol(shp)<=4 && all(vapply(shp@data, FUN = function(x) {
						if (!is.numeric(x)) {
							FALSE
						} else {
							!any(is.na(x)) && min(x)>=0 && max(x<=255)
						}
					}, FUN.VALUE = logical(1))))	
				} else FALSE

				if (convert.RGB) shp@data <- raster_colors(shp)
				raster_facets_vars <- names(shp)[1]
			} else {
				raster_facets_vars <- intersect(raster_facets_vars, names(shp))
			}
			
			## subset data, make factors of non-numeric variables
			#raster_data <- preprocess_raster_data(shp@data, raster_facets_vars)
			
			lvls <- get_data_frame_levels(shp@data[, raster_facets_vars, drop=FALSE])
			
			## use bilinear interpolation for numeric data only
			use_interp <- (all(sapply(lvls, is.null))) && !to.Cat
			shp <- raster::subset(brick(shp), raster_facets_vars, drop=FALSE)
		} else {
			is.OSM <- FALSE
			leaflet.provider <- NA
			
			# color values are encoded by a colortable (and not interpreted as factors)
			if (length(colortable(shp))>0) {
				ctable <- colortable(shp)
				uctable <- unique(ctable)
				mtch <- match(ctable, uctable)

				if (nlayers(shp)>1) shp <- raster::subset(shp, 1)
				shp <- setValues(shp, mtch[getValues(shp) + 1L])
				names(shp) <- "PIXEL__COLOR"
				use_interp <- FALSE
				
				lvls <- list(uctable)
				
			} else {
				# in order to not loose factor levels, subset the data here
				shpnames <- get_raster_names(shp)
				if (is.na(raster_facets_vars[1]) || !any(raster_facets_vars %in% names(shp))) {
					convert.RGB <- if (!identical(is.RGB, FALSE)) {
						(nlayers(shp)>=3 && nlayers(shp)<=4 && minValue(shp)>=0 && maxValue(shp)<= 255)	
					} else FALSE

					if (convert.RGB) {
						pix <- raster_colors(shp)$PIXEL__COLOR
						shp <- raster(shp, layer=0)
						shp <- setValues(shp, as.integer(pix))
						names(shp) <- "PIXEL__COLOR"
						raster_facets_vars <- "PIXEL__COLOR"
						lvls <- list(levels(pix))
					} else raster_facets_vars <- shpnames[1]
				} else {
					convert.RGB <- FALSE
					raster_facets_vars <- intersect(raster_facets_vars, shpnames)
				}
				
				if (!convert.RGB) {
					layerIDs <- match(raster_facets_vars, shpnames)
					lvls <- get_raster_levels(shp, layerIDs)
					
					#raster_data <- get_raster_data(shp)[, raster_facets_vars, drop=FALSE]
					#lvls <- get_data_frame_levels(raster_data)
					# subset raster to get rid of non-used variables (to make projectRaster faster)
					if (nlayers(shp)>1) shp <- raster::subset(shp, raster_facets_vars)
					
					#lvls <- get_raster_levels(shp)
					use_interp <- (all(sapply(lvls, is.null))) && !to.Cat

				} else {
					use_interp <- FALSE
				}
			}
		}
		
		# get current projection (assume longlat if unkown)
		shp_CRS <- get_projection(shp, as.CRS = TRUE)
		if (is.na(shp_CRS)) {
			if (!tmaptools::is_projected(shp)) {
				warning("Currect projection of shape ", y$shp_name, " unknown. Long-lat (WGS84) is assumed.", call. = FALSE)
				shp_CRS <- .CRS_longlat
				shp <- set_projection(shp, current.projection = shp_CRS)
			} else {
				warning("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
			}
		}

		# should raster shape be reprojected?
		if ((!is.na(shp_CRS) && !is.na(gm$shape.master_CRS) && !identical(shp_CRS, gm$shape.master_CRS)) || interactive) {
			if (is.na(gm$shape.master_CRS)) stop("Master projection unknown, but needed to reproject raster shape.", call.=FALSE)
			new_ext <- tryCatch({
			  	suppressWarnings(projectExtent(shp, crs = gm$shape.master_CRS))
			}, error=function(e){
		  		NULL
		  	})
			
			if (is.null(new_ext)) {
				shp <- crop_shape(shp, bb(gm$shape.bbx_raw, projection = shp_CRS, current.projection = gm$shape.master_CRS))	
				new_ext <- tryCatch({
					suppressWarnings(projectExtent(shp, crs = gm$shape.master_CRS))
				}, error=function(e){
					stop("Unable to reproject raster shape \"", y$shp_name, "\", probably due to non-finite points.", call. = FALSE)
				})
			}
		} else new_ext <- NULL

		if (!is.null(new_ext)) {
			shpTmp <- suppressWarnings(projectRaster(shp, to=new_ext, crs=gm$shape.master_CRS, method = ifelse(use_interp, "bilinear", "ngb")))
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
			new_ext_ll <- extent(projectExtent(new_ext, crs = .CRS_longlat))
			shp2@extent <- new_ext_ll
			shp2@crs <- .CRS_longlat
		}
		
		## to be consistent with Spatial objects:
		attr(shp2, "bbox") <- bbox(shp2)
		attr(shp2, "proj4string") <- shp2@crs
		
		attr(data, "is.OSM") <- is.OSM
		attr(data, "leaflet.provider") <- leaflet.provider
		
		#attr(data, "is.RGB") <- is.RGB
		
		type <- "raster"
		
	} else {
		if (inherits(shp, c("sf", "sfc"))) {
			shp <- as(shp, "Spatial")
		} else if (!inherits(shp, "Spatial")) {
			stop("Object ", y$shp_name, " is neither from class Spatial, Raster, nor sf.", call. = FALSE)
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
		shp_CRS <- get_projection(shp, as.CRS = TRUE)
		if (is.na(shp_CRS)) {
			if (!tmaptools::is_projected(shp)) {
				warning("Currect projection of shape ", y$shp_name, " unknown. Long-lat (WGS84) is assumed.", call. = FALSE)
				shp_CRS <- .CRS_longlat
				shp <- set_projection(shp, current.projection = shp_CRS)
			} else {
				warning("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
			}
		}
		if (!is.na(shp_CRS) && !is.na(gm$shape.master_CRS) && !identical(shp_CRS, gm$shape.master_CRS)) {
			shp2 <- tryCatch({
				spTransform(shp, gm$shape.master_CRS)
			}, error=function(e) {
				stop("Unable to project shape ", y$shp_name, " to the projection ", CRSargs(gm$shape.master_CRS), ".", call.=FALSE)
			}, warning=function(w){
				NULL
			})
		} else {
			shp2 <- shp
		}
		
		if (inherits(shp2, "SpatialPolygonsDataFrame")) {
			data$SHAPE_AREAS <- do.call(tmaptools::approx_areas, c(list(shp=shp2, show.warnings=FALSE), shp.aa))
			if (gm$shape.apply_map_coloring) attr(data, "NB") <- if (length(shp)==1) list(0) else poly2nb(shp)
			attr(data, "kernel_density") <- ("kernel_density" %in% names(attributes(shp)))
			type <- "polygons"
		} else if (inherits(shp2, "SpatialLinesDataFrame")) {
			attr(data, "isolines") <- ("isolines" %in% names(attributes(shp)))
			type <- "lines"
		} else if (inherits(shp2, "SpatialPointsDataFrame")) {
			type <- "points"
		}
		
		# simplify shape
		if (shp.sim$fact < 1 && type %in% c("polygons", "lines")) {
			shp2 <- do.call(tmaptools::simplify_shape, c(list(shp=shp2), shp.sim))
			data <- data[shp2$tmapID, , drop=FALSE]
			shp2$tmapID <- seq_len(length(shp2))
		}
		
	}
	attr(shp2, "projected") <- tmaptools::is_projected(shp2)
	
	list(shp=shp2, data=data, type=type)
}