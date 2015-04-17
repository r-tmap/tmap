#' Get ID's of the shape items
#' 
#' Get ID's of the shape items. For polygons and lines, the ID attribute is used. For points, the coordinates are used.
#'
#' @param shp shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#' }
#' @return vector of ID's
#' @export
get_IDs <- function(shp) {
	if (inherits(shp, "SpatialPolygons")) {
		sapply(shp@polygons, function(x)x@ID)
	} else if (inherits(shp, "SpatialLines")) {
		sapply(shp@lines, function(x)x@ID)
	} else if (inherits(shp, "SpatialPixels")) {
		shp@grid.index
	} else if (inherits(shp, "SpatialPoints")) {
		apply(shp@coords, MARGIN=1, function(x) {
			paste(format(round(x, 6), nsmall = 6), collapse="_")
		})
	} else {
		return(NULL)
	}
}