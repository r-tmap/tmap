#' Get ID's of the polygons
#' 
#' Get ID's of the polygons
#'
#' @param shp shape object
#' @return vector of ID's
#' @export
get_IDs <- function(shp) {
	if (inherits(shp, "SpatialPolygons")) {
		sapply(shp@polygons, function(x)x@ID)
	} else if (inherits(shp, "SpatialLines")) {
		sapply(shp@lines, function(x)x@ID)
	} else if (inherits(shp, "SpatialPoints")) {
		apply(shp@coords, MARGIN=1, function(x) {
			paste(format(round(x, 6), nsmall = 6), collapse="_")
		})
	} else {
		return(NULL)
	}
}