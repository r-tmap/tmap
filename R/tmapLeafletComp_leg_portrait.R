tmapLeafletCompPrepare = function(...) {
	UseMethod("tmapLeafletCompPrepare")
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





#' @method tmapLeafletCompPrepare tm_legend_standard_landscape
#' @export
tmapLeafletCompPrepare.tm_legend_standard_landscape = function(comp, o) {
	warning("landscape legends not implemented in 'view' mode; resorting to portrait legends")
	comp
}

#' @method tmapLeafletCompHeight tm_legend_standard_landscape
#' @export
tmapLeafletCompHeight.tm_legend_standard_landscape = function(comp, o) {
	comp
}



#' @method tmapLeafletCompWidth tm_legend_standard_landscape
#' @export
tmapLeafletCompWidth.tm_legend_standard_landscape = function(comp, o) {
	comp
}

