#' Get ID's of the polygons
#' 
#' Get ID's of the polygons
#'
#' @param shp shape object
#' @return vector of ID's
#' @export
get_IDs <- function(shp) {
	sapply(shp@polygons, function(x)x@ID)
}