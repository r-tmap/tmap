tmapLeafletCompPrepare = function(...) {
	UseMethod("tmapGridCompPrepare")
}

tmapLeafletCompHeight = function(...) {
	UseMethod("tmapLeafletCompHeight")
}

tmapLeafletCompWidth = function(...) {
	UseMethod("tmapLeafletCompWidth")
}

tmapLeafletLegPlot = function(...) {
	UseMethod("tmapLeafletLegPlot")
}


#' @method tmapLeafletCompPrepare tm_legend_standard_portrait
#' @export
tmapLeafletCompPrepare.tm_legend_standard_portrait = function(comp, o) {
	comp
}


#' @method tmapLeafletCompHeight tm_legend_standard_portrait
#' @export
tmapLeafletCompHeight.tm_legend_standard_portrait = function(comp, o) {
	comp
}



#' @method tmapLeafletCompWidth tm_legend_standard_portrait
#' @export
tmapLeafletCompWidth.tm_legend_standard_portrait = function(comp, o) {
	comp
}

