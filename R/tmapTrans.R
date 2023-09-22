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

# args:
# - points.only: "yes", "no", "ifany"
tmapTransCentroid = function(shpTM, ord__, plot.order, args) {
	within(shpTM, {
		is_stars = inherits(shp, "dimensions")
		if (is_stars && args$points.only == "no") {
			### stars
			
			s = structure(list(values = matrix(TRUE, nrow = nrow(shp))), dimensions = shp, class = "stars")
			strs = st_as_stars(list(values = m), dimensions = shp)
			shp = sf::st_as_sfc(s, as_points = TRUE)
		} else if (is_stars) {
			shp = st_sfc()
			tmapID = integer(0)
		} else {
			geom_types = sf::st_geometry_type(shp)
			
			ids_poly = which(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
			ids_line = which(geom_types %in% c("LINESTRING", "MULTILINESTRING"))
			ids_point = which(geom_types %in% c("POINT", "MULTIPOINT"))
			
			
			if (args$points.only == "yes" || (args$points.only == "ifany" && length(ids_point))) {
				shp = shp[ids_point]
				tmapID = tmapID[ids_point]
			} else {
				if (length(ids_line)) {
					shp[ids_line] = suppressWarnings({
						sf::st_centroid(shp[ids_line])
					})
				}
				
				if (length(ids_poly)) {
					shp[ids_poly] = suppressWarnings({
						sf::st_centroid(shp[ids_poly])
					})
				}
			}
			rm(geom_types)
		}
	})
}


tmapTransRaster = function(shpTM, ord__, plot.order, args) {
	if (!inherits(shpTM$shp, "dimensions")) stop("Stars object (of class dimensions) expected for tm_raster", call. = FALSE)
	shpTM
}


# args:
# - polygons.only: "yes", "no", "ifany"
tmapTransPolygons = function(shpTM, ord__, plot.order, args) {
	within(shpTM, {
		is_stars = inherits(shp, "dimensions")
		if (is_stars && args$polygons.only == "no") {
			### stars
			s = structure(list(values = matrix(TRUE, nrow = nrow(shp))), dimensions = shp, class = "stars")
			shp = sf::st_as_sfc(s, as_points = FALSE)
		} else if (is_stars) {
			shp = st_sfc()
			tmapID = integer(0)
		} else {
			
			### sf
			geom_types = sf::st_geometry_type(shp)
			#crs = sf::st_crs(shp)
			
			ids_poly = which(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
			ids_line = which(geom_types %in% c("LINESTRING", "MULTILINESTRING"))
			ids_point = which(geom_types %in% c("POINT", "MULTIPOINT"))

			
			if (args$polygons.only == "yes" || (args$polygons.only == "ifany" && length(ids_poly))) {
				shp = shp[ids_poly]
				tmapID = tmapID[ids_poly]
			} else {
				if (length(ids_line)) {
					tryCatch({
						shp[ids_line] = sf::st_cast(sf::st_cast(shp[ids_line], "MULTILINESTRING"), "MULTIPOLYGON")	
					}, error = function(e) {
						stop("Unable to cast lines to polygon. Error from st_cast: \"", e$message, "\"", call. = FALSE)	
					})
				}
				if (length(ids_point)) {
					dist = if (sf::st_is_longlat(crs)) 0.01 else 100
					shp[ids_point] = sf::st_buffer(shp[ids_point], dist = dist)
				}
				
			}
			rm(geom_types)
		}
		
		if (plot.order$aes == "AREA" && !is_stars) {
			o = order(without_units(st_area(shp)), decreasing = !plot.order$reverse)
			shp = shp[o]
			tmapID = tmapID[o]
		}
	})
}

# args:
# - lines.only: "yes", "no", "ifany"
tmapTransLines = function(shpTM, ord__, plot.order, args) {
	within(shpTM, {
		is_stars = inherits(shp, "dimensions")
		if (is_stars) {
			shp = st_sfc()
			tmapID = integer(0)
		} else {
			
			### sf
			geom_types = sf::st_geometry_type(shp)
			#crs = sf::st_crs(shp)
			
			ids_poly = which(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
			ids_line = which(geom_types %in% c("LINESTRING", "MULTILINESTRING"))
			ids_point = which(geom_types %in% c("POINT", "MULTIPOINT"))
			
			
			if (args$lines.only == "yes" || (args$lines.only == "ifany" && length(ids_line))) {
				shp = shp[ids_line]
				tmapID = tmapID[ids_line]
			} else {
				if (length(ids_poly)) {
					tryCatch({
						shp[ids_poly] = sf::st_cast(sf::st_cast(shp[ids_poly], "MULTIPOLYGON"), "MULTILINESTRING")
					}, error = function(e) {
						stop("Unable to cast to polygon. Error from st_cast: \"", e$message, "\"", call. = FALSE)	
					})
				}
				if (length(ids_point)) {
					ids_not_point = which(!(geom_types %in% c("POINT", "MULTIPOINT")))
					shp = shp[ids_not_point]
					tmapID = tmapID[ids_not_point]
				}
				
			}
			rm(geom_types)
		}
		
		if (plot.order$aes == "LENGTH") {
			o = order(without_units(st_length(shp)), decreasing = !plot.order$reverse)
			shp = shp[o]
			tmapID = tmapID[o]
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
	
	rlang::check_installed("cartogram")
	
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
