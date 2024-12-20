tmapReproject = function(...) {
	UseMethod("tmapReproject")
}


reproject_bbox = function(bbox, crs) {
	s = tmaptools::bb_poly(bbox) #st_as_sfc does not add intermediate points
	s2 = sf::st_transform(s, crs)
	sf::st_bbox(s2)
}




#' Internal method that processed shape objects
#'
#' Internal method that processed shape objects
#'
#' @param shp shp
#' @param is.main is.main
#' @param crs crs
#' @param bbox Bounding box
#' @param unit unit
#' @param filter filter
#' @param shp_name shp_name
#' @param smeta smeta
#' @param o o
#' @param tmf tmf
#' @export
#' @import data.table
#' @keywords internal
tmapShape = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	UseMethod("tmapShape")
}

#' Internal method that subsets data from shape objects
#'
#' Internal method that subsets data from shape objects
#'
#' @param shp shape
#' @param vars a vector of variable names
#' @export
#' @keywords internal
tmapSubsetShp = function(shp, vars) {
	UseMethod("tmapSubsetShp")
}

#' Internal method that extracts meta data from shape objects
#'
#' Internal method that extracts meta data from shape objects
#'
#' @param shp the shape object
#' @param o the list of options
#' @export
#' @keywords internal
tmapGetShapeMeta1 = function(shp, o) {
	UseMethod("tmapGetShapeMeta1")
}

#' Internal method that extracts more meta data from shape objects
#'
#' Internal method that extracts meta data from shape objects
#'
#' @param shp the shape
#' @param smeta meta (from `tmapGetShapeMeta1()`)
#' @param o the list of options
#' @export
#' @keywords internal
tmapGetShapeMeta2 = function(shp, smeta, o) {
	UseMethod("tmapGetShapeMeta2")
}


