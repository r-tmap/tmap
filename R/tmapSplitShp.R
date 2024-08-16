#' Internal method that split shape objects
#' 
#' Internal method that split shape objects. So far, only used to split stars object
#' (from dimension to attributes)
#' 
#' @param shp shape
#' @param split_stars_dim name of the dimension to split (`""` to skip)
#' @export
#' @keywords internal
tmapSplitShp = function(shp, split_stars_dim, smeta) {
	UseMethod("tmapSplitShp")
}

#' @export
tmapSplitShp.stars = function(shp, split_stars_dim, smeta) {
	if (split_stars_dim != "") {
		if (length(smeta$vars) > 1L) shp = merge(shp, name = "DIMVARS__")
		
		vals = stars::st_get_dimension_values(shp, split_stars_dim)
		shp = split(shp, split_stars_dim)
		names(shp) = vals
	}
	shp
}

#' @export
tmapSplitShp.default = function(shp, split_stars_dim, smeta) {
	shp
}