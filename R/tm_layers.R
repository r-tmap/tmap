tm_polygons = function(fill = "blue", 
					   fill.setup = tm_aes_color_discrete(), 
					   geom = tm_geom_polygons) {
	tm_element_list(tm_element(as.list(environment()), subclass = "tm_polygons"))
}

tm_raster = function(color = "blue", 
					 color.setup = tm_aes_color_discrete(), 
					 geom = tm_geom_raster) {
	tm_element_list(tm_element(as.list(environment()), subclass = "tm_raster"))
}

tm_symbols = function(color = "blue", 
					  size = 1, 
					  shape = 19, 
					  color.setup = tm_aes_color_discrete(), 
					  size.setup = tm_aes_2d_size(),
					  shape.setup = tm_aes_shape(),
					  geom = tm_geom_centroid) {
	tm_element_list(tm_element(as.list(environment()), subclass = "tm_symbols"))
}

tm_geom_centroid = function(shp) {
	if (inherits(shp, "stars")) {
		### stars
		shp = st_as_sf(shp, as_points = TRUE)
	} else {
		
	}
}


tm_geom_polygons = function(shp) {
	if (inherits(shp, "stars")) {
		### stars
		shp = st_as_sf(shp, as_points = FALSE)
	} else {
		### sf
		geom_types = sf::st_geometry_type(shp)
		crs = sf::st_crs(shp)
		
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
	}
	shp
}


tm_aes_color_discrete = function(n = 5,
								 style = "pretty",
								 palette = "Purples") {
	structure(as.list(environment()), class = c("tm_aes_color_discrete", "tm_aes"))
}

tm_aes_2d_size = function(max = NA,
						  perceptual = FALSE) {
	structure(as.list(environment()), class = c("tm_aes_2d_size", "tm_aes"))
}

tm_aes_shape = function(shapes = c(16:21)) {
	structure(as.list(environment()), class = c("tm_aes_shape", "tm_aes"))
}
