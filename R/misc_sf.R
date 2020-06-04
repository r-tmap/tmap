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

get_centroids <- function(shp, of_largest_polygon = FALSE) {
	co <- try(suppressWarnings(st_coordinates(st_centroid(shp, of_largest_polygon = of_largest_polygon))), silent = TRUE)
	if (inherits(co, "try-error")) {
		if (get("tmapOptions", envir = .TMAP_CACHE)$check.and.fix) {
			shp <- sf::st_make_valid(shp)
			co <- try(suppressWarnings(st_coordinates(st_centroid(shp, of_largest_polygon = of_largest_polygon))))
		} else {
			stop("Shape contains invalid polygons. Please fix it or set tmap_options(check.and.fix = TRUE) and rerun the plot", call. = FALSE)	
		}
	}
	co
}

get_midpoint <- function (coords) {
	dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
	dist_mid <- sum(dist)/2
	dist_cum <- c(0, cumsum(dist))
	end_index <- which(dist_cum > dist_mid)[1]
	start_index <- end_index - 1
	start <- coords[start_index, ]
	end <- coords[end_index, ]
	dist_remaining <- dist_mid - dist_cum[start_index]
	start + (end - start) * (dist_remaining/dist[start_index])
	
}

maybe_longlat <- function(bb) {
	(bb[1] >= -180.1 && bb[3] <= 180.1 && bb[2] >= -90.1 && bb[4] <= 90.1)
}

