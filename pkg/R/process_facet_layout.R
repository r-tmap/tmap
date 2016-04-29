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
	
	if (gmeta$attr.outside) {
		anpc <- gmeta$attr.outside.size
		ext_attr_pos <- gmeta$attr.outside.position
		
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
		dsw <- (dw - between.margin.in * (gmeta$ncol-1)) / gmeta$ncol
		dsh <- (dh - between.margin.in * (gmeta$nrow-1)) / gmeta$nrow
	} else if (gmeta$panel.mode=="one") {
		dsw <- (dw - between.margin.in * (gmeta$ncol-1)) / gmeta$ncol
		dsh <- ((dh - between.margin.in * (gmeta$nrow-1)) / gmeta$nrow) - pSH
	} else {
		dsw <- (dw - between.margin.in * (gmeta$ncol-1)-pSW) / gmeta$ncol
		dsh <- ((dh - between.margin.in * (gmeta$nrow-1)-pSH) / gmeta$nrow)
	}

	
	
	return(list(legH=legH, legW=legW, attrH=attrH, pSH=pSH, pSW=pSW, legmar=legmar, legmarx=legmarx, legmary=legmary, attrmar=attrmar, attrmary=attrmary, between.margin.in=between.margin.in, dsh=dsh, dsw=dsw))
}


process_facet_layout <- function(gmeta, external_legend, sasp, dh, dw, legH, legW, attrH, pSH, pSW, legmar, legmarx, legmary, attrmar, attrmary, between.margin.in, dsh, dsw) {
	panel.mode <- outer.margins <- NULL
	

	## calculate facets and total device aspect ratio
	dasp <- dw/dh
	hasp <- sasp * gmeta$ncol / gmeta$nrow
	
	if (hasp>dasp) {
		fW <- dw
		fH <- dw / hasp
	} else {
		fH <- dh
		fW <- dh * hasp
	}
	
	if (gmeta$panel.mode=="none") {
		gH <- fH + (gmeta$nrow - 1) * between.margin.in + legH + attrH
		gW <- fW + (gmeta$ncol - 1) * between.margin.in + legW
	} else if (gmeta$panel.mode=="one") {
		gH <- fH + gmeta$nrow * pSH + (gmeta$nrow - 1) * between.margin.in + legH + attrH
		gW <- fW + (gmeta$ncol - 1) * between.margin.in + legW
	} else {
		gH <- fH + pSH + between.margin.in * gmeta$nrow + legH + attrH
		gW <- fW + pSW + between.margin.in * gmeta$ncol + legW
	}
	
	gasp <- gW/gH
	
	if (gasp>dasp) {
		xs <- 0
		ys <- convertHeight(unit(dh-(dw / gasp), "inch"), "npc", valueOnly=TRUE)
	} else {
		xs <- convertWidth(unit(dw-(gasp * dh), "inch"), "npc", valueOnly=TRUE)
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
		
		if (panel.mode=="none") {
			colrange <- (1:ncol)*2 + 2
			rowrange <- (1:nrow)*2 + 3
			facetw <- ((1-spc-outerx)-xs-legmarx-between.margin.x*(ncol-1))/ncol
			faceth <- ((1-spc-outery)-ys-legmary-attrmary-between.margin.y*(nrow-1))/nrow
			colws <- c(outer.margins[2], xs/2, legmar[2], rep(c(facetw, between.margin.x), ncol-1), facetw, legmar[4], xs/2, outer.margins[4])
			rowhs <- c(outer.margins[3], ys/2, legmar[3], attrmar[3], rep(c(faceth, between.margin.y), nrow-1), faceth, attrmar[1], legmar[1], ys/2, outer.margins[1])
			
		} else if (panel.mode=="one") {
			colrange <- (1:ncol)*2 + 2
			rowrange <- (1:nrow)*3 + 3
			
			facetw <- ((1-spc-outerx)-xs-legmarx-between.margin.x*(ncol-1))/ncol
			faceth <- ((1-spc-outery)-ys-legmary-attrmary-between.margin.y*(nrow-1))/nrow - panelh
			
			colws <- c(outer.margins[2], xs/2, legmar[2], rep(c(facetw, between.margin.x), ncol-1), facetw, legmar[4], xs/2, outer.margins[4])
			rowhs <- c(outer.margins[3], ys/2, legmar[3], attrmar[3], rep(c(panelh, faceth, between.margin.y), nrow-1), panelh, faceth, attrmar[1], legmar[1], ys/2, outer.margins[1])
			
			
		} else {
			colrange <- (1:ncol)*2 + 4
			rowrange <- (1:nrow)*2 + 5
			
			colpanelrow <- 4
			rowpanelcol <- 4
			
			facetw <- ((1-spc-outerx)-xs-legmarx-between.margin.x*ncol-panelw)/ncol
			faceth <- ((1-spc-outery)-ys-legmary-attrmary-between.margin.y*nrow-panelh)/nrow
			
			colws <- c(outer.margins[2], xs/2, legmar[2], panelw, rep(c(between.margin.x, facetw), ncol), legmar[4], xs/2, outer.margins[4])
			rowhs <- c(outer.margins[3], ys/2, legmar[3], attrmar[3], panelh, rep(c(between.margin.y, faceth), nrow), attrmar[1],legmar[1], ys/2, outer.margins[1])
			
		}
		if (gmeta$legend.outside.position[1] == "left") {
			legx <- 3
			legy <- 5:(length(rowhs)-4)
		} else if (gmeta$legend.outside.position[1] == "right") {
			legx <- length(colws)-2
			legy <- 5:(length(rowhs)-4)
		} else if (gmeta$legend.outside.position[1] == "top") {
			legy <- 3
			legx <- 4:(length(colws)-3)
		} else if (gmeta$legend.outside.position[1] == "bottom") {
			legy <- length(rowhs)-2
			legx <- 4:(length(colws)-3)
		}
		
		if (gmeta$attr.outside.position[1] == "top") {
			attry <- 4
			attrx <- 4:(length(colws)-3)
		} else {
			attry <- length(rowhs)-3
			attrx <- 4:(length(colws)-3)
		}
	})
	gmeta$gasp <- gasp
	gmeta
}
