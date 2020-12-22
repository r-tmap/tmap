tmapTransCentroid = function(tms, ...) {
	within(tms, {
		if (inherits(shp, "stars")) {
			### stars
			shp = sf::st_as_sf(shp, as_points = TRUE)
		} else {
			shp = sf::st_centroid(shp)
		}
	})
}


tmapTransRaster = function(tms, ...) {
	if (!inherits(tms$shp, "stars")) stop("Stars object expected for tm_geom_raster", call. = FALSE)
	tms
}

tmapTransPolygon = function(tms, ...) {
	within(tms, {
		if (inherits(shp, "stars")) {
			### stars
			shp = st_as_sf(shp, as_points = FALSE)
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
	})
}

tmapTransCartogram = function(tms, size, size.setup, ...) {
	if (!requireNamespace("cartogram")) {
		stop("cartogram package required", call. = FALSE)
	}
	tms = tmapTransPolygon(tms)
	within(tms, {
		sizes = dt[[size]]
		shp = cartogram::cartogram_cont(shp, weight = sizes, itermax = 5)
		rm(sizes)
	})
}
