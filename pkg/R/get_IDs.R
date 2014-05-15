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
	} else {
		return(NULL)
	}
}