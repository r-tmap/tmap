process_facet_layout <- function(gm) {
	panel.mode <- outer.margins <- attr.outside.position <- legend.outside.position <- NULL
	
	fpi <- gm$shape.fpi
	
	if (gm$panel.mode=="none") {
		dh2 <- gm$shape.dh - fpi$legH - fpi$attrH - fpi$mainH - (gm$nrow - 1) * fpi$between.margin.in - fpi$xlabHin - gm$nrow * fpi$xgridHin
		dw2 <- gm$shape.dw - fpi$legW - (gm$ncol - 1) * fpi$between.margin.in - fpi$ylabWin - gm$ncol * fpi$ygridWin
	} else if (gm$panel.mode=="one") {
		dh2 <- gm$shape.dh - fpi$legH - fpi$attrH - fpi$mainH - gm$nrow * fpi$pSH - (gm$nrow - 1) * fpi$between.margin.in - fpi$xlabHin - gm$nrow * fpi$xgridHin
		dw2 <- gm$shape.dw - fpi$legW - (gm$ncol - 1) * fpi$between.margin.in - fpi$ylabWin - gm$ncol * fpi$ygridWin
	} else {
		dh2 <- gm$shape.dh - fpi$legH - fpi$attrH - fpi$mainH - fpi$pSH - fpi$between.margin.in * gm$nrow - fpi$xlabHin - gm$nrow * fpi$xgridHin
		dw2 <- gm$shape.dw - fpi$legW - fpi$pSW - fpi$between.margin.in * gm$ncol - fpi$ylabWin - gm$ncol * fpi$ygridWin
	}
	
	## calculate facets and total device aspect ratio
	#dasp <- dw/dh
	dasp2 <- dw2/dh2
	hasp <- gm$shape.sasp * gm$ncol / gm$nrow
	
	if (hasp>dasp2) {
		fW <- dw2
		fH <- dw2 / hasp
	} else {
		fH <- dh2
		fW <- dh2 * hasp
	}
	
	# if (gm$panel.mode=="none") {
	# 	gH <- fH + (gm$nrow - 1) * fpi$between.margin.in + fpi$xlabHin + gm$nrow * fpi$xgridHin
	# 	gW <- fW + (gm$ncol - 1) * fpi$between.margin.in + fpi$ylabWin + gm$ncol * fpi$ygridWin
	# } else if (gm$panel.mode=="one") {
	# 	gH <- fH + gm$nrow * fpi$pSH + (gm$nrow - 1) * fpi$between.margin.in + fpi$xlabHin + gm$nrow * fpi$xgridHin
	# 	gW <- fW + (gm$ncol - 1) * fpi$between.margin.in + fpi$ylabWin + gm$ncol * fpi$ygridWin
	# } else {
	# 	gH <- fH + fpi$pSH + fpi$between.margin.in * gm$nrow + fpi$xlabHin + gm$nrow * fpi$xgridHin
	# 	gW <- fW + fpi$pSW + fpi$between.margin.in * gm$ncol + fpi$ylabWin + gm$ncol * fpi$ygridWin
	# }
	gasp <- fW/fH

	if (gasp>dasp2) {
		xs <- 0
		ys <- convertHeight(unit(dh2-(dw2 / gasp), "inch"), "npc", valueOnly=TRUE)
	} else {
		xs <- convertWidth(unit(dw2-(gasp * dh2), "inch"), "npc", valueOnly=TRUE)
		ys <- 0
	}
	
	outerx <- sum(gm$outer.margins[c(2,4)])
	outery <- sum(gm$outer.margins[c(1,3)])
	
	spc <- 1e-5 # trick also used before (v1.2), to prevent clipping of frame border
	
	gm <- within(gm, {
		between.margin.y <- convertHeight(unit(fpi$between.margin.in, "inch"), "npc", valueOnly=TRUE)
		between.margin.x <- convertWidth(unit(fpi$between.margin.in, "inch"), "npc", valueOnly=TRUE)
		panelh <- convertHeight(unit(fpi$pSH, "inch"), "npc", valueOnly=TRUE)
		panelw <- convertWidth(unit(fpi$pSW, "inch"), "npc", valueOnly=TRUE)

		ylabWnpc <- convertWidth(unit(fpi$ylabWin, "inch"), "npc", valueOnly=TRUE)
		xlabHnpc <- convertHeight(unit(fpi$xlabHin, "inch"), "npc", valueOnly=TRUE)

		ygridWnpc <- convertWidth(unit(fpi$ygridWin, "inch"), "npc", valueOnly=TRUE)
		xgridHnpc <- convertHeight(unit(fpi$xgridHin, "inch"), "npc", valueOnly=TRUE)
		
		attr.between.legend.and.map <- attr.outside.position %in% c("top", "bottom")
		
		
		if (panel.mode=="none") {
			colrange <- (1:ncol)*3 + 3
			rowrange <- (1:nrow)*3 + 3
			facetw <- ((1-spc-outerx)-xs-fpi$legmarx-ylabWnpc-between.margin.x*(ncol-1))/ncol-ygridWnpc
			faceth <- ((1-spc-outery)-ys-fpi$legmary-fpi$attrmary-fpi$mainmary-xlabHnpc-between.margin.y*(nrow-1))/nrow-xgridHnpc
			colws <- c(outer.margins[2], xs/2, fpi$legmar[2], ylabWnpc, rep(c(ygridWnpc, facetw, between.margin.x), ncol-1), ygridWnpc, facetw, fpi$legmar[4], xs/2, outer.margins[4])
			
			if (attr.between.legend.and.map) {
				rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$legmar[3], fpi$attrmar[3], rep(c(faceth, xgridHnpc, between.margin.y), nrow-1), faceth, xgridHnpc, xlabHnpc, fpi$attrmar[1], fpi$legmar[1], ys/2, outer.margins[1])
			} else {
				rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$attrmar[3], fpi$legmar[3], rep(c(faceth, xgridHnpc, between.margin.y), nrow-1), faceth, xgridHnpc, xlabHnpc, fpi$legmar[1], fpi$attrmar[1], ys/2, outer.margins[1])
			}
			
		} else if (panel.mode=="one") {
			colrange <- (1:ncol)*3 + 3
			rowrange <- (1:nrow)*4 + 3
			
			facetw <- ((1-spc-outerx)-xs-fpi$legmarx-ylabWnpc-between.margin.x*(ncol-1))/ncol-ygridWnpc
			faceth <- ((1-spc-outery)-ys-fpi$legmary-fpi$attrmary-fpi$mainmary-xlabHnpc-between.margin.y*(nrow-1))/nrow - panelh-xgridHnpc
			
			colws <- c(outer.margins[2], xs/2, fpi$legmar[2], ylabWnpc, ygridWnpc, rep(c(facetw, between.margin.x, ygridWnpc), ncol-1), facetw, fpi$legmar[4], xs/2, outer.margins[4])
			if (attr.between.legend.and.map) {
				rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$legmar[3], fpi$attrmar[3], rep(c(panelh, faceth, xgridHnpc, between.margin.y), nrow-1), panelh, faceth, xgridHnpc, xlabHnpc, fpi$attrmar[1], fpi$legmar[1], ys/2, outer.margins[1])
			} else {
				rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$attrmar[3], fpi$legmar[3], rep(c(panelh, faceth, xgridHnpc, between.margin.y), nrow-1), panelh, faceth, xgridHnpc, xlabHnpc, fpi$legmar[1], fpi$attrmar[1], ys/2, outer.margins[1])
			}
			
		} else {
			colrange <- (1:ncol)*3 + 5
			rowrange <- (1:nrow)*3 + 5
			
			colpanelrow <- 6
			rowpanelcol <- 5
			
			facetw <- ((1-spc-outerx)-xs-fpi$legmarx-ylabWnpc-between.margin.x*ncol-panelw)/ncol-ygridWnpc
			faceth <- ((1-spc-outery)-ys-fpi$legmary-fpi$attrmary-fpi$mainmary-xlabHnpc-between.margin.y*nrow-panelh)/nrow-xgridHnpc
			
			colws <- c(outer.margins[2], xs/2, fpi$legmar[2], ylabWnpc, panelw, rep(c(between.margin.x, ygridWnpc, facetw), ncol), fpi$legmar[4], xs/2, outer.margins[4])
			
			if (attr.between.legend.and.map) {
				rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$legmar[3], fpi$attrmar[3], panelh, rep(c(between.margin.y, faceth, xgridHnpc), nrow), xlabHnpc, fpi$attrmar[1],fpi$legmar[1], ys/2, outer.margins[1])
			} else {
				rowhs <- c(outer.margins[3], ys/2, fpi$mainmary, fpi$attrmar[3], fpi$legmar[3], panelh, rep(c(between.margin.y, faceth, xgridHnpc), nrow), xgridHnpc, xlabHnpc, fpi$legmar[1], fpi$attrmar[1], ys/2, outer.margins[1])
			}
			
		}
		if (legend.outside.position[1] == "left") {
			legx <- 3
			legy <- 5:(length(rowhs)-5)
		} else if (legend.outside.position[1] == "right") {
			legx <- length(colws)-2
			legy <- 5:(length(rowhs)-5)
		} else if (legend.outside.position[1] == "top") {
			legy <- 5- attr.between.legend.and.map
			legx <- 5:(length(colws)-3)
		} else if (legend.outside.position[1] == "bottom") {
			legy <- length(rowhs)-3 + attr.between.legend.and.map
			legx <- 5:(length(colws)-3)
		}
		
		if (tolower(attr.outside.position[1]) == "top") {
			attry <- 3 + attr.between.legend.and.map
			attrx <- 5:(length(colws)-3)
		} else {
			attry <- length(rowhs)-2 - attr.between.legend.and.map
			attrx <- 5:(length(colws)-3)
		}
		
		xlaby <- length(rowhs)-4
		xlabx <- 5:(length(colws)-3)
		
		ylaby <- 5:(length(rowhs)-5)
		ylabx <- 4
		
	})
	
	gm$gasp <- unname(gasp)
	gm
}
