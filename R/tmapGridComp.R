
#' @method tmapGridCompPrepare tm_title
#' @export
tmapGridCompPrepare.tm_title = function(comp, o) {
	comp
}


#' @method tmapGridCompHeight tm_title
#' @export
tmapGridCompHeight.tm_title = function(comp, o) {
	titleS = if (comp$title == "") 0 else comp$size #* o$scale
	titleP = comp$padding[c(3,1)] * titleS * o$lin
	titleH = titleS * o$lin
	comp$Hin = sum(titleP[1], titleH, titleP[2])
	comp
}

#' @method tmapGridCompWidth tm_title
#' @export
tmapGridCompWidth.tm_title = function(comp, o) {
	titleS = if (comp$title == "") 0 else comp$size #* o$scale
	titleP = comp$padding[c(2,4)] * titleS * o$lin
	titleW = titleS * strwidth(comp$title, units = "inch", family = comp$fontfamily, font = fontface2nr(comp$fontface))
	comp$Win = sum(titleP[1], titleW, titleP[2])
	comp
}


#' @method tmapGridLegPlot tm_title
#' @export
tmapGridLegPlot.tm_title = function(comp, o) {
	textS = comp$text.size * comp$scale #* o$scale
	titleS = if (comp$title == "") 0 else comp$size * comp$scale #* o$scale
	
	padding = grid::unit(comp$padding[c(3,4,1,2)] * titleS * o$lin, units = "inch")
	
	if (comp$position$align.h == "left") {
		#x = grid::unit(0, "npc") 
		x = grid::unit(comp$padding[2] * titleS * o$lin, units = "inch")
		halign = 0
		hjust = 1
		just = "left"
	} else if (comp$position$align.h == "right") {
		#x = grid::unit(1, "npc") 
		x = grid::unit(1, "npc") - grid::unit(comp$padding[4] * titleS * o$lin, units = "inch")
		halign = 1
		hjust = 0
		just = "right"
	} else {
		x = grid::unit(0.5, "npc")
		halign = 0.5
		hjust = 0.5
		just = "center"
	}
	
	# grTitle = gridtext::richtext_grob(comp$title, 
	# 								  x = x,
	# 								  box_gp = gpar(col = frame.col, fill = bg.color, alpha = bg.alpha, lwd = frame.lwd),
	# 								  r = grid::unit(frame.r, "pt"),
	# 								  halign = halign,
	# 								  hjust = hjust, 
	# 								  gp = grid::gpar(cex = titleS))
	grTitle = grid::textGrob(comp$title, 
							 x = x,
							 just = just,
							 gp = grid::gpar(col = comp$color, cex = titleS))
	
	if (getOption("tmap.design.mode")) {
		grDesign = grid::rectGrob(gp=gpar(fill=NA,col="red", lwd=2))
	} else {
		grDesign = NULL
	}
	
	g = do.call(grid::grobTree, c(list(grTitle), list(grDesign))) #, list(vp = vp)
	
	g
	
}
