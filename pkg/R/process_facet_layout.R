preprocess_facet_layout <- function(gmeta, external_legend, dh, dw) {
	between.margin.in <- convertHeight(unit(gmeta$between.margin, "lines"), "inch", valueOnly=TRUE) * gmeta$scale
	
	if (external_legend) {
		lnpc <- gmeta$legend.outside.size
		ext_leg_pos <- gmeta$legend.outside.position[1]
		
		if (ext_leg_pos == "left") {
			legmar <- c(0, lnpc, 0, 0)
		} else if (ext_leg_pos == "right") {
			legmar <- c(0, 0, 0, lnpc)
		} else if (ext_leg_pos == "top") {
			legmar <- c(0, 0, lnpc, 0)
		} else if (ext_leg_pos == "bottom") {
			legmar <- c(lnpc, 0, 0, 0)
		}
		
	} else {
		legmar <- rep(0, 4)
	}
	legmarx <- sum(legmar[c(2,4)])
	legmary <- sum(legmar[c(1,3)])
	legW <- convertWidth(unit(legmarx, "npc"), "inch", valueOnly=TRUE)
	legH <- convertHeight(unit(legmary, "npc"), "inch", valueOnly=TRUE)
	
	if (gmeta$xlab.show) {
		xlabHin <- convertHeight(unit(gmeta$xlab.size, "lines")*1.25, "inch", valueOnly=TRUE)
	} else {
		xlabHin <- 0
	}
	
	if (gmeta$ylab.show) {
		ylabWin <- convertHeight(unit(gmeta$ylab.size, "lines")*1.25, "inch", valueOnly=TRUE)
	} else {
		ylabWin <- 0
	}
	
	if (gmeta$attr.outside) {
		anpc <- gmeta$attr.outside.size
		ext_attr_pos <- tolower(gmeta$attr.outside.position)
		
		if (ext_attr_pos == "top") {
			attrmar <- c(0, 0, anpc, 0)
		} else {
			attrmar <- c(anpc, 0, 0, 0)
		}
	} else {
		attrmar <- rep(0, 4)
	}
	attrmary <- sum(attrmar[c(1,3)])
	attrH <- convertHeight(unit(attrmary, "npc"), "inch", valueOnly=TRUE)
	
	pS <-  convertHeight(unit(gmeta$panel.label.size, "lines"), "inch", valueOnly=TRUE) * gmeta$panel.label.height
	
	pSH <- if (gmeta$panel.show) {
		ifelse(gmeta$panel.label.rot[2]==0, pS, {
			panelnames <- if (is.list(gmeta$panel.names)) gmeta$panel.names[[2]] else gmeta$panel.names
			max(convertWidth(stringWidth(panelnames), "inch", valueOnly=TRUE) * 1.25 * gmeta$panel.label.size)
		})
	} else 0
	
	pSW <- if (is.list(gmeta$panel.names) && gmeta$panel.show) {
			ifelse(gmeta$panel.label.rot[1]==90, pS, {
				max(convertWidth(stringWidth(gmeta$panel.names[[1]]), "inch", valueOnly=TRUE) * 1.25 * gmeta$panel.label.size)
			})
	} else 0
	
	
	# calculate facet device size
	if (gmeta$panel.mode=="none") {
		dsw <- (dw - between.margin.in * (gmeta$ncol-1) - legW) / gmeta$ncol
		dsh <- (dh - between.margin.in * (gmeta$nrow-1) - legH - attrH) / gmeta$nrow
	} else if (gmeta$panel.mode=="one") {
		dsw <- (dw - between.margin.in * (gmeta$ncol-1) - legW) / gmeta$ncol
		dsh <- ((dh - between.margin.in * (gmeta$nrow-1) - legH - attrH) / gmeta$nrow) - pSH
	} else {
		dsw <- (dw - between.margin.in * (gmeta$ncol-1)-pSW - legW) / gmeta$ncol
		dsh <- ((dh - between.margin.in * (gmeta$nrow-1)-pSH - legH - attrH) / gmeta$nrow)
	}

	
	
	return(list(legH=legH, legW=legW, attrH=attrH, pSH=pSH, pSW=pSW, legmar=legmar, legmarx=legmarx, legmary=legmary, attrmar=attrmar, attrmary=attrmary, xlabHin=xlabHin, ylabWin=ylabWin, between.margin.in=between.margin.in, dsh=dsh, dsw=dsw))
}


process_facet_layout <- function(gmeta, external_legend, sasp, dh, dw, legH, legW, attrH, pSH, pSW, legmar, legmarx, legmary, attrmar, attrmary, xlabHin, ylabWin, between.margin.in, dsh, dsw) {
	panel.mode <- outer.margins <- NULL
	
	dh2 <- dh - legH - attrH
	dw2 <- dw - legW
	
	## calculate facets and total device aspect ratio
	#dasp <- dw/dh
	dasp2 <- dw2/dh2
	hasp <- sasp * gmeta$ncol / gmeta$nrow
	
	if (hasp>dasp2) {
		fW <- dw2
		fH <- dw2 / hasp
	} else {
		fH <- dh2
		fW <- dh2 * hasp
	}
	
	if (gmeta$panel.mode=="none") {
		gH <- fH + (gmeta$nrow - 1) * between.margin.in + xlabHin
		gW <- fW + (gmeta$ncol - 1) * between.margin.in + ylabWin
	} else if (gmeta$panel.mode=="one") {
		gH <- fH + gmeta$nrow * pSH + (gmeta$nrow - 1) * between.margin.in + xlabHin
		gW <- fW + (gmeta$ncol - 1) * between.margin.in + ylabWin
	} else {
		gH <- fH + pSH + between.margin.in * gmeta$nrow + xlabHin
		gW <- fW + pSW + between.margin.in * gmeta$ncol + ylabWin
	}
	gasp <- gW/gH

	if (gasp>dasp2) {
		xs <- 0
		ys <- convertHeight(unit(dh2-(dw2 / gasp), "inch"), "npc", valueOnly=TRUE)
	} else {
		xs <- convertWidth(unit(dw2-(gasp * dh2), "inch"), "npc", valueOnly=TRUE)
		ys <- 0
	}
	outerx <- sum(gmeta$outer.margins[c(2,4)])
	outery <- sum(gmeta$outer.margins[c(1,3)])
	
	spc <- 1e-5 # trick also used before (v1.2), to prevent clipping of frame border
	
	gmeta <- within(gmeta, {
		between.margin.y <- convertHeight(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE)
		between.margin.x <- convertWidth(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE)
		panelh <- convertHeight(unit(pSH, "inch"), "npc", valueOnly=TRUE)
		panelw <- convertWidth(unit(pSW, "inch"), "npc", valueOnly=TRUE)

		ylabWnpc <- convertWidth(unit(ylabWin, "inch"), "npc", valueOnly=TRUE)
		xlabHnpc <- convertHeight(unit(xlabHin, "inch"), "npc", valueOnly=TRUE)
		
		attr.between.legend.and.map <- attr.outside.position %in% c("top", "bottom")
		
		
		if (panel.mode=="none") {
			colrange <- (1:ncol)*2 + 3
			rowrange <- (1:nrow)*2 + 3
			facetw <- ((1-spc-outerx)-xs-legmarx-ylabWnpc-between.margin.x*(ncol-1))/ncol
			faceth <- ((1-spc-outery)-ys-legmary-attrmary-xlabHnpc-between.margin.y*(nrow-1))/nrow
			colws <- c(outer.margins[2], xs/2, legmar[2], ylabWnpc, rep(c(facetw, between.margin.x), ncol-1), facetw, legmar[4], xs/2, outer.margins[4])
			
			if (attr.between.legend.and.map) {
				rowhs <- c(outer.margins[3], ys/2, legmar[3], attrmar[3], rep(c(faceth, between.margin.y), nrow-1), faceth, xlabHnpc, attrmar[1], legmar[1], ys/2, outer.margins[1])
			} else {
				rowhs <- c(outer.margins[3], ys/2, attrmar[3], legmar[3], rep(c(faceth, between.margin.y), nrow-1), faceth, xlabHnpc, legmar[1], attrmar[1], ys/2, outer.margins[1])
			}
			

		} else if (panel.mode=="one") {
			colrange <- (1:ncol)*2 + 3
			rowrange <- (1:nrow)*3 + 3
			
			facetw <- ((1-spc-outerx)-xs-legmarx-ylabWnpc-between.margin.x*(ncol-1))/ncol
			faceth <- ((1-spc-outery)-ys-legmary-attrmary-xlabHnpc-between.margin.y*(nrow-1))/nrow - panelh
			
			colws <- c(outer.margins[2], xs/2, legmar[2], ylabWnpc, rep(c(facetw, between.margin.x), ncol-1), facetw, legmar[4], xs/2, outer.margins[4])
			if (attr.between.legend.and.map) {
				rowhs <- c(outer.margins[3], ys/2, legmar[3], attrmar[3], rep(c(panelh, faceth, between.margin.y), nrow-1), panelh, faceth, xlabHnpc, attrmar[1], legmar[1], ys/2, outer.margins[1])
			} else {
				rowhs <- c(outer.margins[3], ys/2, attrmar[3], legmar[3], rep(c(panelh, faceth, between.margin.y), nrow-1), panelh, faceth, xlabHnpc, legmar[1], attrmar[1], ys/2, outer.margins[1])
			}
			
		} else {
			colrange <- (1:ncol)*2 + 5
			rowrange <- (1:nrow)*2 + 5
			
			colpanelrow <- 5
			rowpanelcol <- 4
			
			facetw <- ((1-spc-outerx)-xs-legmarx-ylabWnpc-between.margin.x*ncol-panelw)/ncol
			faceth <- ((1-spc-outery)-ys-legmary-attrmary-xlabHnpc-between.margin.y*nrow-panelh)/nrow
			
			colws <- c(outer.margins[2], xs/2, legmar[2], ylabWnpc, panelw, rep(c(between.margin.x, facetw), ncol), legmar[4], xs/2, outer.margins[4])
			
			if (attr.between.legend.and.map) {
				rowhs <- c(outer.margins[3], ys/2, legmar[3], attrmar[3], panelh, rep(c(between.margin.y, faceth), nrow), xlabHnpc, attrmar[1],legmar[1], ys/2, outer.margins[1])
			} else {
				rowhs <- c(outer.margins[3], ys/2, attrmar[3], legmar[3], panelh, rep(c(between.margin.y, faceth), nrow), xlabHnpc, legmar[1], attrmar[1], ys/2, outer.margins[1])
			}
			
		}
		if (legend.outside.position[1] == "left") {
			legx <- 3
			legy <- 5:(length(rowhs)-5)
		} else if (legend.outside.position[1] == "right") {
			legx <- length(colws)-2
			legy <- 5:(length(rowhs)-5)
		} else if (legend.outside.position[1] == "top") {
			legy <- 4- attr.between.legend.and.map
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
	gmeta$gasp <- gasp
	gmeta
}
