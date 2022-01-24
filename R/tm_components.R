tm_title = function(title, size, padding) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	structure(args, class = c("tm_title", "tm_component", "list"))
	
	
	g <- list(tm_comp=as.list(environment()))
	names(g$tm_credits) <- paste("credits", names(g$tm_credits), sep=".")
	class(g) <- "tmap"
	attr(g, "call") <- names(match.call(expand.dots = TRUE)[-1])
	g
}

#' @method tmapGridCompPrepare tm_title
#' @export
tmapGridCompPrepare.tm_title = function(leg, o) {
	leg
}


#' @method tmapGridCompHeight tm_title
#' @export
tmapGridCompHeight.tm_title = function(leg, o) {
	titleS = if (leg$title == "") 0 else leg$title.size
	titleP = leg$title.padding[c(3,1)] * titleS * o$lin
	titleH = titleS * o$lin
	leg$Hin = sum(titleP[1], titleH, titleP[2])
	leg
}

#' @method tmapGridCompWidth tm_title
#' @export
tmapGridCompWidth.tm_title = function(leg, o) {
	titleS = if (leg$title == "") 0 else leg$title.size
	titleP = leg$title.padding[c(2,4)] * titleS * o$lin
	titleW = titleS * strwidth(leg$title, units = "inch", family = leg$title.fontfamily, font = fontface2nr(leg$title.fontface)) * o$lin
	leg$Win = sum(titleP[1], titleW, titleP[2])
	leg
}


#' @method tmapGridLegPlot tm_title
#' @export
tmapGridLegPlot.tm_title = function(leg, o) {
	textS = leg$text.size * leg$scale
	titleS = if (leg$title == "") 0 else leg$title.size * leg$scale
	
	if (leg$title.just == "left") {
		grTitle = grid::textGrob(leg$title, x = grid::unit(leg$title.padding[2] * titleS * o$lin, units = "inch"), just = "left", gp = grid::gpar(cex = titleS))
	} else if (leg$title.just == "right") {
		grTitle = grid::textGrob(leg$title, x = grid::unit(1, "npc") - grid::unit(leg$title.padding[4] * titleS * o$lin, units = "inch"), just = "right", gp = grid::gpar(cex = titleS))
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
