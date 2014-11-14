get_polygon_directions <- function(shp, y0_bottom=TRUE) {
	lapply(shp@polygons, function(P) {
		sapply(P@Polygons, function(p) {
			co <- coordinates(p)
			n <- nrow(co)
			res <- (sum((co[-(n), 1] - co[-1, 1]) * (co[-1, 2] + co[-(n), 2])) > 0)
			if (!y0_bottom) res <- !res
			res
		})
	})
}


set_polygon_directions <- function(shp, y0_bottom=TRUE, clockwise=TRUE) {
	shp@polygons <- lapply(shp@polygons, function(P) {
		P@Polygons <- lapply(P@Polygons, function(p) {
			co <- coordinates(p)
			n <- nrow(co)
			res <- (sum((co[-(n), 1] - co[-1, 1]) * (co[-1, 2] + co[-(n), 2])) > 0)
			if (!y0_bottom) res <- !res
			if (res != clockwise) {
				p@coords <- co[n:1,]
			}
			p
		})
		P
	})	
	shp
}