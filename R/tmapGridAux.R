findZoom = function(b) {
	## calculate zoom level
	# borrowed from https://github.com/dkahle/ggmap/blob/master/R/calc_zoom.r
	lon_diff = b[3] - b[1]
	lat_diff = b[4] - b[2]

	zoomlon = ceiling(log2(360 * 2/lon_diff))
	zoomlat = ceiling(log2(180 * 2/lat_diff))
	zoom = as.integer(min(zoomlon, zoomlat))
}


#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPrepare = function(a, bs, id, o) {
	UseMethod("tmapGridAuxPrepare")
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPrepare.default = function(a, bs, id, o) {
	""
}


#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPlot = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	UseMethod("tmapGridAuxPlot")
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPlot.default = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	NULL
}
