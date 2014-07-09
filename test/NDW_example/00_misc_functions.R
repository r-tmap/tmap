get_lengths <- function(shp, digits=3) {
	round(SpatialLinesLengths(shp, longlat=FALSE)/1000, digits=digits)
}