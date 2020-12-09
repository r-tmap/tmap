assign_values = function(shp, dt, column) {
	shp[[1]][dt$tmapID__] = dt[[column]]
	shp
}

get_main_crs = function(tmls) {
	is_main = vapply(tmls, function(tme) {
		identical(tme$is.main, TRUE)
	}, FUN.VALUE = logical(1))
	
	main_id = if (any(is_main)) which(is_main)[1L] else 1L
	
	tms_main = tmls[[main_id]]
	
	crs_main = tms_main$crs
	if (is.null(crs_main)) crs_main = sf::st_crs(tms_main$shp)
	crs_main
}
