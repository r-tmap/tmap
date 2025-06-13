#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridDataPlot = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	UseMethod("tmapGridDataPlot")
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridDataPlot.default = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NULL
}
