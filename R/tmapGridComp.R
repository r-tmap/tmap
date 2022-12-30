#' @method tmapGridCompPrepare tm_title
#' @export
tmapGridCompPrepare.tm_title = function(comp, o) {
	comp
}

#' @method tmapGridCompHeight tm_title
#' @export
tmapGridCompHeight.tm_title = function(comp, o) {
	tmapGridCompHeight_text(comp, o)
}

#' @method tmapGridCompWidth tm_title
#' @export
tmapGridCompWidth.tm_title = function(comp, o) {
	tmapGridCompWidth_text(comp, o)
}

#' @method tmapGridLegPlot tm_title
#' @export
tmapGridLegPlot.tm_title = function(comp, o) {
	tmapGridLegPlot_text(comp, o)
}

#' @method tmapGridCompPrepare tm_credits
#' @export
tmapGridCompPrepare.tm_credits = function(comp, o) {
	comp
}

#' @method tmapGridCompHeight tm_credits
#' @export
tmapGridCompHeight.tm_credits = function(comp, o) {
	tmapGridCompHeight_text(comp, o)
}

#' @method tmapGridCompWidth tm_credits
#' @export
tmapGridCompWidth.tm_credits = function(comp, o) {
	tmapGridCompWidth_text(comp, o)
}

#' @method tmapGridLegPlot tm_credits
#' @export
tmapGridLegPlot.tm_credits = function(comp, o) {
	tmapGridLegPlot_text(comp, o)
}


tmapGridLegPlot_text = function(comp, o) {
	textS = if (comp$text == "") 0 else comp$size * comp$scale #* o$scale
	
	padding = grid::unit(comp$padding[c(3,4,1,2)] * textS * o$lin, units = "inch")
	
	if (comp$position$align.h == "left") {
		#x = grid::unit(0, "npc") 
		x = grid::unit(comp$padding[2] * textS * o$lin, units = "inch")
		halign = 0
		hjust = 1
		just = "left"
	} else if (comp$position$align.h == "right") {
		#x = grid::unit(1, "npc") 
		x = grid::unit(1, "npc") - grid::unit(comp$padding[4] * textS * o$lin, units = "inch")
		halign = 1
		hjust = 0
		just = "right"
	} else {
		x = grid::unit(0.5, "npc")
		halign = 0.5
		hjust = 0.5
		just = "center"
	}
	
	# grtext = gridtext::richtext_grob(comp$text, 
	# 								  x = x,
	# 								  box_gp = gpar(col = frame.col, fill = bg.color, alpha = bg.alpha, lwd = frame.lwd),
	# 								  r = grid::unit(frame.r, "pt"),
	# 								  halign = halign,
	# 								  hjust = hjust, 
	# 								  gp = grid::gpar(cex = textS))
	grtext = grid::textGrob(comp$text, 
							 x = x,
							 just = just,
							 gp = grid::gpar(col = comp$color, cex = textS))
	
	if (getOption("tmap.design.mode")) {
		grDesign = grid::rectGrob(gp=gpar(fill=NA,col="red", lwd=2))
	} else {
		grDesign = NULL
	}
	
	g = do.call(grid::grobTree, c(list(grtext), list(grDesign))) #, list(vp = vp)
	
	g
	
}

tmapGridCompHeight_text = function(comp, o) {
	textS = if (comp$text == "") 0 else comp$size #* o$scale
	textP = comp$padding[c(3,1)] * textS * o$lin
	textH = textS * o$lin
	comp$Hin = sum(textP[1], textH, textP[2])
	comp
}

tmapGridCompWidth_text = function(comp, o) {
	textS = if (comp$text == "") 0 else comp$size #* o$scale
	textP = comp$padding[c(2,4)] * textS * o$lin
	textW = textS * strwidth(comp$text, units = "inch", family = comp$fontfamily, font = fontface2nr(comp$fontface))
	comp$Win = sum(textP[1], textW, textP[2])
	comp
}





