pre_process_facet_layout <- function(gm, external_legend, dh, dw) {
	between.margin.in <- convertHeight(unit(gm$between.margin, "lines"), "inch", valueOnly=TRUE) * gm$scale
	
	if (external_legend) {
		lnpc <- gm$legend.outside.size
		ext_leg_pos <- gm$legend.outside.position[1]
		
		if (ext_leg_pos == "left") {
			legmar <- c(0, lnpc, 0, 0)
		} else if (ext_leg_pos == "right") {
			legmar <- c(0, 0, 0, lnpc)
		} else if (ext_leg_pos == "top") {
			legmar <- c(0, 0, lnpc, 0)
		} else if (ext_leg_pos == "bottom") {
			legmar <- c(lnpc, 0, 0, 0)
		} else {
			stop("Invalid legend.outside.position value. Should be one of \"left\", \"right\", \"top\", \"bottom\"", call. = FALSE)
		}
		
	} else {
		legmar <- rep(0, 4)
	}
	legmarx <- sum(legmar[c(2,4)])
	legmary <- sum(legmar[c(1,3)])
	legW <- convertWidth(unit(legmarx, "npc"), "inch", valueOnly=TRUE)
	legH <- convertHeight(unit(legmary, "npc"), "inch", valueOnly=TRUE)
	
	if (gm$xlab.show) {
		nlines <- gm$xlab.nlines + gm$xlab.space
		xlabHin <- convertHeight(unit(gm$xlab.size, "lines")*(nlines*1.25), "inch", valueOnly=TRUE)
	} else {
		xlabHin <- 0
	}
	
	if (gm$ylab.show) {
		nlines <- gm$ylab.nlines + gm$ylab.space
		ylabWin <- convertHeight(unit(gm$ylab.size, "lines")*(nlines*1.25), "inch", valueOnly=TRUE)
	} else {
		ylabWin <- 0
	}
	
	if (gm$grid.show && !gm$grid.labels.inside.frame) {
		
		xbbstringWin <- max(convertWidth(stringWidth(do.call("fancy_breaks", c(list(vec=gm$shape.bbx_cropped[c(1,3)], intervals=FALSE), gm$grid.labels.format))), "inch", valueOnly = TRUE)) * gm$grid.labels.size
		ybbstringWin <- max(convertWidth(stringWidth(do.call("fancy_breaks", c(list(vec=gm$shape.bbx_cropped[c(2,4)], intervals=FALSE), gm$grid.labels.format))), "inch", valueOnly = TRUE)) * gm$grid.labels.size
		
		lineH <- convertHeight(unit(gm$grid.labels.size, "lines"), "inch", valueOnly=TRUE)
		
		xgridHin <- ifelse(!is.na(gm$grid.labels.space.x), gm$grid.labels.space.x * lineH, ifelse(gm$grid.labels.rot[1] %in% c(0, 180), 1.25 * lineH, xbbstringWin + lineH * .5) + gm$grid.labels.margin.x * lineH)
		ygridWin <- ifelse(!is.na(gm$grid.labels.space.y), gm$grid.labels.space.y * lineH, ifelse(gm$grid.labels.rot[2] %in% c(0, 180), ybbstringWin + lineH * .5, 1.25 * lineH) + gm$grid.labels.margin.y * lineH)
	} else {
		xgridHin <- 0
		ygridWin <- 0
	}
	
	
	
	
	if (gm$attr.outside) {
		anpc <- gm$attr.outside.size
		ext_attr_pos <- tolower(gm$attr.outside.position)
		
		if (ext_attr_pos == "top") {
			attrmar <- c(0, 0, anpc, 0)
		} else {
			attrmar <- c(anpc, 0, 0, 0)
		}
	} else {
		attrmar <- rep(0, 4)
	}
	attrmary <- sum(attrmar[c(1,3)])
	attrH <- convertHeight(unit(attrmary, "npc"), "inch", valueOnly = TRUE)
	
	mainTitleLines <- max(vapply(gm$main.title, function(mt) {
		if (mt==0) 0 else number_text_lines(mt)
	}, numeric(1)))
	mainH <- convertHeight(unit(mainTitleLines, "lines")*1.2*gm$main.title.size, "inch", valueOnly=TRUE)
	mainmary <- convertHeight(unit(mainH, "inch"), "npc", valueOnly = TRUE)

	pS <-  convertHeight(unit(gm$panel.label.size, "lines"), "inch", valueOnly=TRUE) * gm$panel.label.height
	
	pSH <- if (gm$panel.show) {
		ifelse(gm$panel.label.rot[2]==0, pS, {
			panelnames <- if (is.list(gm$panel.names)) gm$panel.names[[2]] else gm$panel.names
			max(convertWidth(stringWidth(panelnames), "inch", valueOnly=TRUE) * 1.25 * gm$panel.label.size)
		})
	} else 0
	
	pSW <- if (is.list(gm$panel.names) && gm$panel.show) {
		ifelse(gm$panel.label.rot[1]==90, pS, {
			max(convertWidth(stringWidth(gm$panel.names[[1]]), "inch", valueOnly=TRUE) * 1.25 * gm$panel.label.size)
		})
	} else 0
	
	
	# calculate facet device size
	if (gm$panel.mode=="none") {
		dsw <- (dw - between.margin.in * (gm$ncol-1) - ygridWin * gm$ncol - legW) / gm$ncol
		dsh <- (dh - between.margin.in * (gm$nrow-1) - xgridHin * gm$nrow - legH - attrH - mainH) / gm$nrow
	} else if (gm$panel.mode=="one") {
		dsw <- (dw - between.margin.in * (gm$ncol-1) - legW) / gm$ncol
		dsh <- ((dh - between.margin.in * (gm$nrow-1) - legH - attrH - mainH) / gm$nrow) - pSH
	} else {
		dsw <- (dw - between.margin.in * (gm$ncol-1)-pSW - legW) / gm$ncol
		dsh <- ((dh - between.margin.in * (gm$nrow-1)-pSH - legH - attrH - mainH) / gm$nrow)
	}
	
	
	
	return(list(legH=legH, legW=legW, attrH=attrH, mainH=mainH, pSH=pSH, pSW=pSW, legmar=legmar, legmarx=legmarx, legmary=legmary, attrmar=attrmar, attrmary=attrmary, mainmary=mainmary, xlabHin=xlabHin, ylabWin=ylabWin, xgridHin=xgridHin, ygridWin=ygridWin, between.margin.in=between.margin.in, dsh=dsh, dsw=dsw))
}
