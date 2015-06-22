#' Get aspect ratio
#' 
#' Get the aspect ratio of a shape object, i.e., the map width devided by the map height.
#' @param shp shape object, either \code{\link[sp:Spatial]{Spatial}} or a \code{\link[raster:Raster-class]{Raster}}.
#' @return aspect ratio
#' @import sp
#' @export
get_asp_ratio <- function(shp) {
	bb <- bbox(shp)
	calc_asp_ratio(bb[1, ], bb[2, ], !is_projected(shp))
}

calc_asp_ratio <- function(xlim, ylim, longlat) {
	if (is.na(longlat)) longlat <- TRUE
	unname((diff(xlim)/diff(ylim)) * ifelse(longlat, cos((mean(ylim) * pi)/180), 1))
}