tm_title = function(title, size, padding, fontface, fontfamily, stack, just, frame, frame.lwd, frame.r, position, width, height) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	tm_element_list(tm_element(
		args = args,
		subclass = c("tm_title", "tm_component")))
}



# if (!is.na(o$title)) {
# 	legs = legs[1:3]
# 	legs$legend[[3]] = structure(list(title = o$title, padding = c(.1,.1,.1,.1), size = o$size, fontface = o$fontface, fontfamily = o$fontfamily, stack = o$legend.stack, just = "left", position = o$legend.position), class = c("tm_title", "tm_legend", "list"))
# 	legs$class[[3]] = "autoout"
# 	legs$cell.h[3] = "right"
# 	legs$cell.v[3] = "bottom"
# }

#' @method tmapGridCompPrepare tm_title
#' @export
tmapGridCompPrepare.tm_title = function(leg, o) {
	leg
}


#' @method tmapGridCompHeight tm_title
#' @export
tmapGridCompHeight.tm_title = function(leg, o) {
	titleS = if (leg$title == "") 0 else leg$size
	titleP = leg$padding[c(3,1)] * titleS * o$lin
	titleH = titleS * o$lin
	leg$Hin = sum(titleP[1], titleH, titleP[2])
	leg
}

#' @method tmapGridCompWidth tm_title
#' @export
tmapGridCompWidth.tm_title = function(leg, o) {
	titleS = if (leg$title == "") 0 else leg$size
	titleP = leg$padding[c(2,4)] * titleS * o$lin
	titleW = titleS * strwidth(leg$title, units = "inch", family = leg$fontfamily, font = fontface2nr(leg$fontface)) * o$lin
	leg$Win = sum(titleP[1], titleW, titleP[2])
	leg
}


#' @method tmapGridLegPlot tm_title
#' @export
tmapGridLegPlot.tm_title = function(leg, o) {
	textS = leg$text.size * leg$scale
	titleS = if (leg$title == "") 0 else leg$size * leg$scale
	
	if (leg$just == "left") {
		grTitle = grid::textGrob(leg$title, x = grid::unit(leg$padding[2] * titleS * o$lin, units = "inch"), just = "left", gp = grid::gpar(cex = titleS))
	} else if (leg$just == "right") {
		grTitle = grid::textGrob(leg$title, x = grid::unit(1, "npc") - grid::unit(leg$padding[4] * titleS * o$lin, units = "inch"), just = "right", gp = grid::gpar(cex = titleS))
	} else {
		grTitle = grid::textGrob(leg$title, x = 0.5, just = "center", gp = grid::gpar(cex = titleS))
	}
	
	if (getOption("tmap.design.mode")) {
		grDesign = grid::rectGrob(gp=gpar(fill=NA,col="red", lwd=2))
	} else {
		grDesign = NULL
	}
	
	g = do.call(grid::grobTree, c(list(grTitle), grDesign)) #, list(vp = vp)
	
	g
	
}
