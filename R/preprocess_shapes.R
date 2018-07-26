rasterCheckSize <- function(x, interactive) {
	# if (maxpixels < raster::ncell(x)) {
	# 	warning(paste("maximum number of pixels for Raster* viewing is",
	# 				  maxpixels, "; \nthe supplied Raster* has", ncell(x), "\n",
	# 				  "... decreasing Raster* resolution to", maxpixels, "pixels\n",
	# 				  "to view full resolution set 'maxpixels = ", ncell(x), "'"))

	tmapOptions <- get(".tmapOptions", envir = .TMAP_CACHE)
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
	
	show.messages <- get(".tmapOptions", envir = .TMAP_CACHE)$show.messages

	if (is.null(shp)) return(list(shp=NULL, data=NULL, type="tiles"))

	shp.unit <- gm$shape.unit
	# shp.aa <- y[c("unit", "orig", "to", "total.area")]
	# names(shp.aa)[names(shp.aa)=="unit"] <- "target"
	# shp.aa <- shp.aa[!sapply(shp.aa, is.null)]
	
	shp.sim <- y[c("simplify", "keep.units", "keep.subunits", "method", "no_repair", "snap", "force_FC", "drop_null_geometries")]
	names(shp.sim)[names(shp.sim)=="simplify"] <- "fact"
	shp.sim <- shp.sim[!vapply(shp.sim, is.null, logical(1))]
	
	if (inherits(shp, c("Raster", "SpatialPixels", "SpatialGrid"))) {
		is.RGB <- attr(raster_facets_vars, "is.RGB") # true if tm_rgb is used (NA if qtm is used)
		to.Cat <- attr(raster_facets_vars, "to.Cat") # true if tm_raster(..., style = "cat) is specified

		if (interactive) gm$shape.master_crs <- .crs_merc
		
		if (inherits(shp, "Spatial")) shp <- brick(shp)
			
		
		
		# attribute get from read_osm
		is.OSM <- attr(shp, "is.OSM")
		if (is.null(is.OSM)) is.OSM <- FALSE
		leaflet.server <- attr(shp, "leaflet.provider")
		if (is.null(leaflet.server)) leaflet.server <- NA
		
		# color values are encoded by a colortable (and not interpreted as factors)
		if (length(colortable(shp))>0) {
			ctable <- colortable(shp)
			uctable <- unique(ctable)
			mtch <- match(ctable, uctable)

			if (nlayers(shp)>1) shp <- raster::subset(shp, 1)
			shp <- setValues(shp, mtch[getValues(shp) + 1L])
			names(shp) <- shpnames <- "PIXEL__COLOR"
			
			
			use_interp <- FALSE
			
			lvls <- list(uctable)
			
			# if (!is.RGB && is.na(do.interpolate)) {
			# 	if (get(".tmapOptions", envir = .TMAP_CACHE)$show.messages) {
			# 		message("For bitmap images, it is recommended to use tm_rgb instead of tm_raster (or to set interpolate to TRUE).")
			# 	}
			# }
			layerIDs <- 1
			convert.RGB <- FALSE
		} else {
			# in order to not loose factor levels, subset the data here
			rdata <- get_raster_data(shp, show.warnings = FALSE)
			mainID <- attr(rdata, "mainID")
			
			
			shpnames <- names(rdata) #get_raster_names(shp)
			
			convert.RGB <- !identical(is.RGB, FALSE) && 
				(is.na(raster_facets_vars[1]) || !any(raster_facets_vars %in% shpnames)) &&
				nlayers(shp)>=3 && nlayers(shp)<=4 && all(minValue(shp)>=0) && all(maxValue(shp)<= 255)
			
			
			if (is.na(is.RGB) && convert.RGB && get(".tmapOptions", envir = .TMAP_CACHE)$show.messages) {
				message("Numeric values of ", y$shp_name, " interpreted as RGB(A) values. Run tm_shape(", y$shp_name, ") + tm_raster() to visualize the data.")
			}
			
			
			if (identical(is.RGB, TRUE) && !convert.RGB) {
				stop("Raster object does not have a color table, nor numeric data that can be converted to colors. Use tm_raster to visualize the data.", call. = FALSE)
			}
			
			if (convert.RGB) {
				layerIDs <- 1L:nlayers(shp)
			} else {
				if (is.na(raster_facets_vars[1])) {
					if (length(mainID) != length(shpnames) && show.messages) {
						if (attr(rdata, "cls") == "RasterLayer") {
							message("Only the first variable is shown. The available variables are: \"", paste(shpnames, collapse = "\", \""), "\".")
						} else {
							message("For each raster layer, only the first variable is shown. The available variables are: \"", paste(shpnames, collapse = "\", \""), "\".")	
						}
					}
					raster_facets_vars <- shpnames[mainID]
				} else {
					raster_facets_vars <- na.omit(raster_facets_vars)
				}
					
				layerIDs <- match(raster_facets_vars, shpnames)
				layerIDs <- na.omit(layerIDs)
				if (length(layerIDs) == 0L) layerIDs <- 1
			}
				#lvls <- get_raster_levels(shp, layerIDs)
			lvls <- get_data_frame_levels(rdata[, layerIDs, drop = FALSE])
				
			use_interp <- ((all(vapply(lvls, is.null, logical(1)))) && !to.Cat)
		}

		#print(shp)
		
		shp <- rasterCheckSize(shp, interactive)	
		
		# print(use_interp)
		# print(lvls)
		# print(layerIDs)
		# print(use_interp)
		#shp <- rasterCheckSize(shp)	
		
		
		# get current projection (assume longlat if unkown)
		shp_crs <- get_projection(shp, output="crs")
		if (is.na(shp_crs)) {
			if (!tmaptools::is_projected(shp)) {
				warning("Currect projection of shape ", y$shp_name, " unknown. Long-lat (WGS84) is assumed.", call. = FALSE)
				shp_crs <- .crs_longlat
				shp <- set_projection(shp, current.projection = shp_crs)
			} else {
				warning("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
			}
		}

		# should raster shape be reprojected?
		if ((!is.na(shp_crs) && !is.na(gm$shape.master_crs) && !identical(shp_crs$proj4string, gm$shape.master_crs$proj4string)) || interactive) {
			if (is.na(gm$shape.master_crs)) stop("Master projection unknown, but needed to reproject raster shape.", call.=FALSE)
			new_ext <- tryCatch({
			  	suppressWarnings(projectExtent(shp, crs = gm$shape.master_crs$proj4string))
			}, error=function(e){
		  		NULL
		  	})
			
			if (is.null(new_ext)) {
				shp <- crop_shape(shp, bb(gm$shape.bbx_raw, projection = shp_crs, current.projection = gm$shape.master_crs))	
				new_ext <- tryCatch({
					suppressWarnings(projectExtent(shp, crs = gm$shape.master_crs$proj4string))
				}, error=function(e){
					stop("Unable to reproject raster shape \"", y$shp_name, "\", probably due to non-finite points.", call. = FALSE)
				})
			}
		} else new_ext <- NULL

		
		raster.projected <- !is.null(new_ext)
		
		if (raster.projected) {
			shpTmp <- suppressWarnings(projectRaster(shp, to=new_ext, crs=gm$shape.master_crs$proj4string, method = ifelse(use_interp, "bilinear", "ngb")))
			shp2 <- raster(shpTmp)
			data <- suppressWarnings(get_raster_data(shpTmp)[,layerIDs, drop=FALSE])
		} else {
			shp2 <- raster(shp)
			data <- suppressWarnings(get_raster_data(shp)[,layerIDs, drop=FALSE])
		}
		
		# restore factor levels and limits
		data <- as.data.frame(mapply(function(d, l) {
			if (is.character(l) && !is.factor(d)) {
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
			} else if (is.numeric(l)) {
				pmin(pmax(l[1], d), l[2])
			} else d
		}, data, lvls, SIMPLIFY=FALSE))
		
		if (convert.RGB) {
			data <- data.frame(PIXEL__COLOR = raster_colors(as.matrix(data)))
		}
		

		# set values in order to align data later on (with cropping)
		shp2 <- setValues(shp2, values=1:ncell(shp2))

		# interactive raster require merc projection with longlat extent		
		if (interactive) {
			new_ext_ll <- extent(projectExtent(new_ext, crs = .crs_longlat$proj4string))
			shp2@extent <- new_ext_ll
			shp2@crs <- raster::crs(.crs_longlat$proj4string)
		}
		
		## to be consistent with Spatial objects:
		attr(shp2, "bbox") <- bb(shp2)
		attr(shp2, "proj4string") <- attr(shp2@crs, "projargs")

		
		data$tmapfilter <- TRUE
		
		attr(data, "is.OSM") <- is.OSM
		attr(data, "leaflet.server") <- leaflet.server
		attr(data, "raster.projected") <- raster.projected
		
		#attr(data, "is.RGB") <- is.RGB
		
		type <- "raster"
		
	} else {
		# save_bbox (sp objects allow for custom bboxes, sf objects don't)
		shp_bbx <- bb(shp)
		
		kernel_density <- ("kernel_density" %in% names(attributes(shp)))
		isolines <- ("isolines" %in% names(attributes(shp)))
		
		if (inherits(shp, "Spatial")) {
			shp <- as(shp, "sf")
		} else if (!inherits(shp, c("sf", "sfc"))) {
			stop("Object ", y$shp_name, " is neither from class sf, Spatial, nor Raster.", call. = FALSE)
		}
		
		# remove empty units
		empty_units <- st_is_empty(shp)
		if (any(empty_units)) {
			shp <- if (inherits(shp, "sf")) shp[!empty_units, ] else shp[!empty_units]
		}
		
		
		## get data.frame from shapes, and store ID numbers in shape objects (needed for cropping)
		if (inherits(shp, "sfc")) {
			data <- data.frame(tmapID = seq_len(length(shp)))
			shp <- st_sf(data, geometry=shp)
		} else {
			data <- shp
			st_geometry(data) <- NULL
			shp <- shp[, attr(shp, "sf_column")]
			shp$tmapID <- seq_len(nrow(shp))
		}
		
		data$tmapfilter <- if (is.null(y$filter)) rep(TRUE, nrow(shp)) else rep(y$filter, length.out = nrow(shp))
		
		# reproject if nessesary
		shp_crs <- get_projection(shp, output="crs")
		if (is.na(shp_crs)) {
			if (!tmaptools::is_projected(shp)) {
				warning("Currect projection of shape ", y$shp_name, " unknown. Long-lat (WGS84) is assumed.", call. = FALSE)
				shp_crs <- .crs_longlat
				shp <- set_projection(shp, current.projection = shp_crs)
			} else {
				warning("Current projection of shape ", y$shp_name, " unknown and cannot be determined.", call. = FALSE)
			}
		}
		if (!is.na(shp_crs) && !is.na(gm$shape.master_crs) && !identical(shp_crs$proj4string, gm$shape.master_crs$proj4string)) {
			shp2 <- set_projection(shp, gm$shape.master_crs)

			# override bounding box (since it now is projected)
			shp_bbx <- bb(shp2)
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
			
			types <- factor(rep(NA, nrow(shp2)), levels=c("polygons", "lines", "points"))
			types[st_is(shp2, c("MULTIPOLYGON", "POLYGON"))] <- "polygons"
			types[st_is(shp2, c("MULTILINESTRING", "LINESTRING"))] <- "lines"
			types[st_is(shp2, c("MULTIPOINT", "POINT"))] <- "points"
			
			type <- "geometrycollection"
			attr(data, "kernel_density") <- FALSE
			attr(type, "types") <- types
			
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
		attr(shp2, "proj4string") <- st_crs(shp2)
		
		shpnames <- names(data)
		
	}
	
	point.per <- if (is.na(y$point.per)) ifelse(type %in% c("points", "geometrycollection"), "segment", "feature") else y$point.per

	attr(data, "shpnames") <- shpnames
	
	attr(shp2, "point.per") <- point.per
	attr(shp2, "line.center") <- y$line.center
	attr(shp2, "projected") <- tmaptools::is_projected(shp2)
	list(shp=shp2, data=data, type=type)
}