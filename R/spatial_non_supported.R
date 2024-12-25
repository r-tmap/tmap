#' @export
tmapGetShapeMeta1.default = function(shp, o) {
	stop("Specified shp argument of tm_shape is a ", class(shp)[1], ", which is not a recognized/supported spatial data class.", call. = FALSE)
}

#' @export
tmapGetShapeMeta2.default = function(shp, smeta, o) {
	stop("Specified shp argument of tm_shape is a ", class(shp)[1], ", which is not a recognized/supported spatial data class.", call. = FALSE)
}

#' @export
tmapSubsetShp.default = function(shp, vars) {
	stop("Specified shp argument of tm_shape is a ", class(shp)[1], ", which is not a recognized/supported spatial data class.", call. = FALSE)
}

#' @export
tmapShape.default = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	stop("Specified shp argument of tm_shape is a ", class(shp)[1], ", which is not a recognized/supported spatial data class.", call. = FALSE)
}
