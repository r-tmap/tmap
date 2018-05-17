sf_split_type <- function(shp) {
	types <- st_geometry_type(shp)	
	
	types2 <- ifelse(types %in% c("MULTIPOLYGON", "POLYGON"), "polygons",
					 ifelse(types %in% c("MULTILINESTRING", "LINESTRING"), "lines", "points"))
	
	list(polygons = shp[types2=="polygons",],
		 lines = shp[types2=="lines",],
		 points = shp[types2=="points",])
}

sf_expand <- function(shp) {
	x <- mapply(function(tp, ge) {
		if (tp == "MULTILINESTRING") {
			st_cast(st_geometry(ge), "LINESTRING")
		} else if (tp == "MULTIPOLYGON") {
			st_cast(st_geometry(ge), "POLYGON")
		} else if (tp == "MULTIPOINT") {
			st_cast(st_geometry(ge), "POINT")
		} else {
			st_geometry(ge)
		}
	}, st_geometry_type(shp), st_geometry(shp), SIMPLIFY = FALSE)
	ids <- rep(1L:length(x), vapply(x, length, integer(1)))
	shp3 <- st_sf(geometry=st_sfc(do.call(c, x)))
	shp3$split__id <- ids
	shp3
}
