#' Get and set polygon direction
#' 
#' Get and set the direction of polygons (clockwise or counterclockwise). (Experimental, see ntoe)
#'
#' @param shp shape object, a SpatialPolygons(DataFrame)
#' @param y0_bottom logical: is the \code{y=0} coordinate at the bottom of the screen?
#' @return List of logical vectors, each corresponding to a Polygons object. The values of each vector correspond to the Polygon objects. \code{TRUE} means clockwise.
#' @export
#' @name get_polygon_directions
#' @note This function is still in experimental phase, which means that it may not be stable and it may be changed significantly in future versions. Moreover, it is unsure if it will stay in tmap; instead, it may be put in a different package, along with functions of similar tasks.
#' @rdname get_polygon_directions
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

#' @param clockwise logical; should the direction of the polygons be set to clockwise?
#' @name set_polygon_directions
#' @rdname get_polygon_directions
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