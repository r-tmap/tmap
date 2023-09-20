#' Internal method that split shape objects
#' 
#' Internal method that split shape objects. So far, only used to split stars object
#' (from dimension to attributes)
#' 
#' @param shp shape
#' @param split_stars_dim name of the dimension to split (`""` to skip)
#' @export
#' @keywords internal
tmapSplitShp = function(shp, split_stars_dim) {
	UseMethod("tmapSplitShp")
}

#' @method tmapSplitShp stars
#' @export
tmapSplitShp.stars = function(shp, split_stars_dim) {
	if (split_stars_dim != "") {
		vals = stars::st_get_dimension_values(shp, split_stars_dim)
		shp = split(shp, split_stars_dim)
		names(shp) = vals
	}
	shp
}

#' @method tmapSplitShp default
#' @export
tmapSplitShp.default = function(shp, split_stars_dim) {
	shp
}