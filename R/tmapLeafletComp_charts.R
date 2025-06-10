#' @method tmapLeafletCompPrepare tm_chart
#' @export
tmapLeafletCompPrepare.tm_chart = function(comp, o) {
	message("charts not implemented in view mode")
	comp
}

#' @method tmapLeafletCompPrepare tm_chart_none
#' @export
tmapLeafletCompPrepare.tm_chart_none = function(comp, o) {
	comp
}


#' @method tmapLeafletCompWidth tm_chart
#' @export
tmapLeafletCompWidth.tm_chart = function(comp, o) {
	comp
}

#' @method tmapLeafletCompHeight tm_chart
#' @export
tmapLeafletCompHeight.tm_chart = function(comp, o) {
	comp
}

#' @method tmapLeafletCompPlot tm_chart_histogram
#' @export
tmapLeafletCompPlot.tm_chart_histogram = function(comp, lf, o) {
	lf
}
#' @method tmapLeafletCompPlot tm_chart
#' @export
tmapLeafletCompPlot.tm_chart = function(comp, lf, o) {
	lf
}
