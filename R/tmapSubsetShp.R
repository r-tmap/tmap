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



#' @method tmapSubsetShp stars
#' @export
tmapSubsetShp.sf = function(shp, vars) {
	if (!length(vars)) {
		vars = "dummy__"
		shp$dummy__ = TRUE
	}
	shp[, vars]
}
