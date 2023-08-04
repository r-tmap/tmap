do_trans = function(tdt, FUN, shpDT, plot.order, args) {
	#browser()
	
	shpDT = copy(shpDT)
	
	# copy by columns from tdt that do not yet exist in shpDT
	t_by = names(tdt)[substr(names(tdt), 1, 2) == "by"]
	s_by = names(shpDT)[substr(names(shpDT), 1, 2) == "by"]
	n_by = setdiff(t_by, s_by)
	if (length(n_by)) {
		shpDT[, (n_by) := tdt[1, n_by, with = FALSE]]
	}
	
	aesvars = setdiff(names(tdt), c("tmapID__", paste0("by", 1:3, "__")))
	
	apply_trans = function(shpTM) {
		# todo: stars
		ids = intersect(shpTM$tmapID, tdt$tmapID__)
		
		shp = shpTM$shp[match(ids, shpTM$tmapID)]
		
		shpX = list(shp = shp, tmapID = ids)
		
		x = as.list(tdt[match(tmapID__, ids), aesvars, with = FALSE])
		
		res = do.call(FUN, c(list(shpTM = shpX), x, list(plot.order = plot.order, args = args)))
	}
	shpDT$shpTM = lapply(shpDT$shpTM, apply_trans)
	list(shpDT)
}


tmapTransCentroid = function(shpTM, ord__, plot.order, args) {
	within(shpTM, {
		if (inherits(shp, "stars")) {
			### stars
			shp = sf::st_as_sf(shp, as_points = TRUE)
		} else {
			geom_types = sf::st_geometry_type(shp)
			is_poly_or_line = geom_types %in% c("POLYGON", "MULTIPOLYGON", "LINESTRING", "MULTILINESTRING")
			is_point = geom_types %in% c("POINT", "MULTIPOINT")

			if (any(is_point) && any(is_poly_or_line)) {
				shp = shp[is_point]
				tmapID = tmapID[is_point]
			} else if (any(is_poly_or_line)) {
				shp[is_poly_or_line] = suppressWarnings({
					sf::st_centroid(shp[is_poly_or_line])
				})
			}
		}
	})
}


tmapTransRaster = function(shpTM, ord__, plot.order, args) {
	if (!inherits(shpTM$shp, "dimensions")) stop("Stars object (of class dimensions) expected for tm_raster", call. = FALSE)
	shpTM
}

tmapTransPolygons = function(shpTM, ord__, plot.order, args) {
	within(shpTM, {
		is_stars = inherits(shp, "dimensions")
		if (is_stars) {
			### stars
			s = structure(list(tmapID = matrix(tmapID, nrow = nrow(shp))), dimensions = shp, class = "stars")
			shp = sf::st_geometry(st_as_sf(s, as_points = FALSE))
		} else {
			### sf
			geom_types = sf::st_geometry_type(shp)
			#crs = sf::st_crs(shp)
			
			if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
				if (all(geom_types %in% c("LINESTRING", "MULTILINESTRING"))) {
					### sf lines
					tryCatch({
						shp = sf::st_cast(sf::st_cast(shp, "MULTILINESTRING"), "MULTIPOLYGON")	
					}, error = function(e) {
						stop("Unable to cast to polygon. Error from st_cast: \"", e$message, "\"", call. = FALSE)	
					})
				} else if (all(geom_types %in% c("POINT", "MULTIPOINT"))) {
					### sf points
					dist = if (sf::st_is_longlat(crs)) 0.01 else 100
					shp = sf::st_buffer(shp, dist = dist)
				} else {
					### sf geometry collection
					stop("Other geometry types, or a collection of geometry types not implemented yet", call. = FALSE)	
				}
			}
			rm(geom_types)
		}
		
		if (plot.order$aes == "AREA" && !is_stars) {
			o = order(units::drop_units(st_area(shp)), decreasing = !plot.order$reverse)
			shp = shp[o]
			tmapID = tmapID[o]
		}
	})
}

tmapTransLines = function(shpTM, ord__, plot.order, args) {
	within(shpTM, {
		is_stars = inherits(shp, "dimensions")
		if (is_stars) {
			### stars
			stop("lines cannot be drawn from a spatial raster", call. = FALSE)
		} else {
			### sf
			geom_types = sf::st_geometry_type(shp)
			#crs = sf::st_crs(shp)
			if (!all(geom_types %in% c("LINESTRING", "MULTILINESTRING"))) {
				if (all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
					### sf lines
					tryCatch({
						shp = sf::st_cast(sf::st_cast(shp, "MULTIPOLYGON"), "MULTILINESTRING")
					}, error = function(e) {
						stop("Unable to cast to polygon. Error from st_cast: \"", e$message, "\"", call. = FALSE)	
					})
				} else {
					### sf geometry collection
					ids = which(geom_types %in% c("LINESTRING", "MULTILINESTRING"))
					shp = shp[ids]
					tmapID = tmapID[ids]
					
				}
			}
			rm(geom_types)
		}
	})
}

tmapTransCartogram = function(shpTM, area, ord__, plot.order, args) {
	s = shpTM$shp
	
	if (sf::st_is_longlat(s)) {
		stop("tm_cartogram requires projected coordinates, not longlat degrees. A projected CRS can be specified in tm_shape (argument crs)", call. = FALSE)
	}

	message("Cartogram in progress...")
		
	x = sf::st_sf(geometry = s, weight = area, tmapID__ = shpTM$tmapID)
	require(cartogram)
	
	if (!requireNamespace("cartogram")) stop("cartogram pacakge needed", call. = FALSE)
	
	if (args$type == "cont") {
		shp = suppressMessages(suppressWarnings({cartogram::cartogram_cont(x, weight = "weight", itermax = args$itermax)}))
	} else if (args$type == "ncont") {
		shp = suppressMessages(suppressWarnings({cartogram::cartogram_ncont(x, weight = "weight")}))
	} else if (args$type == "dorling") {
		shp = suppressMessages(suppressWarnings({cartogram::cartogram_dorling(x, weight = "weight")}))
	} else {
		stop("unknown cartogram type", call. = FALSE)
	}
	
	
	shp2 = sf::st_cast(sf::st_geometry(shp), "MULTIPOLYGON")
	
	ord2 = ord__[match(shpTM$tmapID, shp$tmapID__)]
	
	o = order(ord2, decreasing = FALSE)
	
	list(shp = shp2[o], tmapID = shp$tmapID__[o])
}
