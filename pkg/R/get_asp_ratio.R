#' Get aspect ratio
#' 
#' Get the aspect ratio of a shape object, i.e., the map width devided by the map height.
#' @param shp shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialGridDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPixelsDataFrame]{SpatialPixels(DataFrame)}}}
#' }
#' @return aspect ratio
#' @export
get_asp_ratio <- function(shp) {
	bb <- shp@bbox
	calc_asp_ratio(bb[1, ], bb[2, ], !is_projected(shp))
}

calc_asp_ratio <- function(xlim, ylim, longlat) {
	if (is.na(longlat)) longlat <- TRUE
	(diff(xlim)/diff(ylim)) * ifelse(longlat, cos((mean(ylim) * pi)/180), 1)
}