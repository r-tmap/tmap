tmapSubsetShp = function(...) {
	UseMethod("tmapSubsetShp")
}

#' @method tmapSubsetShp stars
#' @export
tmapSubsetShp.stars = function(shp, vars) {
	ids = unique(c(which(names(shp) %in% vars),
				   which(make.names(names(shp)) %in% vars)))
	shp[ids]
}

#' @method tmapSubsetShp SpatRaster
#' @export
tmapSubsetShp.SpatRaster = function(shp, vars) {
	shp[[vars]]
}

#' @method tmapSubsetShp SpatRaster
#' @export
tmapSubsetShp.Raster = function(shp, vars) {
	tmapSubsetShp.SpatRaster(terra::rast(shp), vars)
}

#' @method tmapSubsetShp SpatVector
#' @export
tmapSubsetShp.SpatVector = function(shp, vars) {
	tmapSubsetShp.sf(shp, vars)
}


#' @method tmapSubsetShp sf
#' @export
tmapSubsetShp.sf = function(shp, vars) {
	if (!length(vars)) {
		vars = "dummy__"
		shp$dummy__ = TRUE
	}
	shp[, vars]
}

#' @method tmapSubsetShp sfc
#' @export
tmapSubsetShp.sfc = function(shp, vars) {
	sf::st_sf(dummy__ = TRUE, geometry = shp)
}
