#' @export
tmapShape.Spatial = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	tmapShape.sf(as(shp, "sf"), is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf)
}

#' @export
tmapSubsetShp.Spatial = function(shp, vars) {
	tmapSubsetShp.sf(as(shp, "sf"), vars)
}


#' @export
tmapGetShapeMeta1.Spatial = function(shp, o) {
	tmapGetShapeMeta1.SpatRaster(as(shp, "sf"), o)
}
