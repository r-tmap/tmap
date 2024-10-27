auto_crs = function(x, world_crs = "+proj=eck4") {
	if (identical(x, TRUE)) return(world_crs)

	is_ll = sf::st_is_longlat(x)
	if (is.na(is_ll)) {
		x = suppressWarnings(sf::st_crs(x) <- st_crs('OGC:CRS84'))
	} else if (!is_ll) {
		x = sf::st_transform(x = x, crs = st_crs('OGC:CRS84'))
	}

	b = sf::st_bbox(x)

	lat_mean = (b["ymax"] + b["ymin"]) / 2
	lon_mean = (b["xmax"] + b["xmin"]) / 2


	earth_surface = 5.1e14

	area = b |>
		sf::st_as_sfc() |>
		sf::st_area() |>
		as.numeric()

	if (area > earth_surface * 0.25 && lat_mean > -50 && lat_mean < 50) {
		world_crs
	} else {
		paste0("+proj=ortho +lat_0=", round(lat_mean), " +lon_0=", round(lon_mean))
	}

}
