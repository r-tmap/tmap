#' Transforms rd(rijksdriekhoekstelsel)-based shape object to wgs84-based shape object.
#' 
#' This function transforms a rd object to a wgs84 object, that is used by Google Earth
#'
#' @param shp object in rd-coordinates
#' @return shp object in wgs84-coordinates
#'
#' @export
#' @example ../examples/rd2wgs84.R
rd2wgs84 <- function(shp) {
	
	rd <- CRS("+init=epsg:28992 +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812")
	wgs84 <- CRS("+proj=longlat +datum=WGS84")

	proj4string(shp) <- rd
	
	spTransform(shp, wgs84)
}