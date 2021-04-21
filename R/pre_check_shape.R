pre_check_shape <- function(shp, name, show.warnings, check.class = TRUE, drop.zm = TRUE, check.valid = TRUE, remove.empty = TRUE, reprojected = FALSE) {
	if (check.class) {
		if (inherits(shp, "Spatial")) {
			shp <- as(shp, "sf")
		} else if (!inherits(shp, c("sf", "sfc"))) {
			stop("Object ", name, " is neither from class sf, stars, Spatial, nor Raster.", call. = FALSE)
		}		
	}
	
	# drop z/m
	if (drop.zm) {
		shp <- sf::st_zm(shp)
	}
	
	add = ifelse(reprojected, " (after reprojection).", ".")
	
	if (check.valid) {
		if (get("tmapOptions", envir = .TMAP_CACHE)$check.and.fix) {
			# check if shp is valid (if not, fix it with a warning)
			if (!all(st_is_valid(shp))) {
				tryCatch({
					shp <- sf::st_make_valid(shp)
					if (show.warnings) {
						warning("The shape ", name, " is invalid", add, " See sf::st_is_valid", call. = FALSE)
					}
					
				}, error = function(e) {
					stop("Unable to make ", name, " valid with sf::st_make_valid", add, call. = FALSE)
				})
			}
		}
	}
	

	# remove empty units
	if (remove.empty) {
		empty_units <- st_is_empty(shp)
		if (all(empty_units)) {
			stop("The shape ", name, " only contains empty units", add, call. = FALSE)
		} else if (any(empty_units)) {
			if (show.warnings) warning("The shape ", name, " contains empty units", add, call. = FALSE)
			shp <- if (inherits(shp, "sf")) shp[!empty_units, ] else shp[!empty_units]
		}
	}

	shp
}
