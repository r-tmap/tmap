pre_check_shape <- function(shp, name, show.warnings) {
	if (inherits(shp, "Spatial")) {
		shp <- as(shp, "sf")
	} else if (!inherits(shp, c("sf", "sfc"))) {
		stop("Object ", name, " is neither from class sf, stars, Spatial, nor Raster.", call. = FALSE)
	}		
	
	# drop z/m
	shp <- sf::st_zm(shp)
	
	
	if (get("tmapOptions", envir = .TMAP_CACHE)$check.and.fix) {
		# check if shp is valid (if not, fix it with a warning)
		if (!all(st_is_valid(shp))) {
			tryCatch({
				shp <- sf::st_make_valid(shp)
				if (show.warnings) warning("The shape ", name, " is invalid. See sf::st_is_valid", call. = FALSE)
			}, error = function(e) {
				stop("Unable to make ", name, " valid with sf::st_make_valid", call. = FALSE)
			})
		}
	}

	# remove empty units
	empty_units <- st_is_empty(shp)
	if (all(empty_units)) {
		stop("The shape ", name, " only contains empty units.", call. = FALSE)
	} else if (any(empty_units)) {
		if (show.warnings) warning("The shape ", name, " contains empty units.", call. = FALSE)
		shp <- if (inherits(shp, "sf")) shp[!empty_units, ] else shp[!empty_units]
	}

	shp
}
