#' @export
tmapShape.Raster = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	tmapShape.SpatRaster(terra::rast(shp), is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf)
}

#' @export
tmapSubsetShp.Raster = function(shp, vars) {
	tmapSubsetShp.SpatRaster(terra::rast(shp), vars)
}

#' @export
tmapGetShapeMeta1.Raster = function(shp, o) {
	tmapGetShapeMeta1.SpatRaster(terra::rast(shp), o)
}
