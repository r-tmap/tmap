
plot_map <- function(i, gp, gt, shps.env) {
	nlayers <- length(gp)
	
	shps <- get("shps", envir=shps.env)
	
	if (any(gt$shp_nr!=0) && gt$free.coords) {
		shps <- shps[[i]]
	}
	
	
	bubbleHeight <- convertHeight(unit(.5, "lines"), "inch", valueOnly=TRUE)
	
	bb <- shps[[1]]@bbox
	
	if (gt$grid.show) {
		treeGrid <- plot_grid(gt, bb, scale=gt$scale)
	} else {
		treeGrid <- NULL
	}
	
	treeElements <- mapply(function(gpl, shp) {
		bb <- shp@bbox
		if (inherits(shp, "SpatialLines")) {
			co <- gCentroid(shp, byid=TRUE)@coords
		} else {
			co <- coordinates(shp)
		}
		co.npc <- co
		co.npc[,1] <- (co.npc[,1]-bb[1,1]) / (bb[1, 2]-bb[1,1])
		co.npc[,2] <- (co.npc[,2]-bb[2,1]) / (bb[2, 2]-bb[2,1])
		
		
		plot_tm_fill <- function() {
			fill <- if (is.null(gpl$fill)) NA else get_alpha_col(gpl$fill, gpl$fill.alpha)
			col <- get_alpha_col(gpl$col, gpl$alpha)
			grid.shape(shp, gp=gpar(fill=fill, col=col, lwd=gpl$lwd, ltw=gpl$lty), bg.col=gt$bg.color)
		}
		
		plot_tm_lines <- function() {
			col <- get_alpha_col(gpl$line.col, gpl$line.alpha)
			grid.shplines(shp, gp=gpar(col=col, lwd=gpl$line.lwd, lty=gpl$line.lty,
									   lineend="butt"))
		}
		
		plot_tm_bubbles <- function() plot_bubbles(co.npc, gpl, bubbleHeight)
		plot_tm_text <- function() plot_text(co.npc, gpl)
		e <- environment()
		fnames <- paste("plot", gpl$plot.order, sep="_")
		grobs <- lapply(fnames, do.call, args=list(), envir=e)
		items <- do.call("gList", args =  grobs)
		gTree(children=items)
	}, gp, shps, SIMPLIFY=FALSE)
	
	grobsElemGrid <- if (gt$grid.show && gt$grid.on.top) {
		do.call("gList", args = c(treeElements, list(treeGrid)))
	} else {
		do.call("gList", args = c(list(treeGrid), treeElements))
	}
	list(treeElemGrid=gTree(children=grobsElemGrid, name="mapElements"), bubbleHeight=bubbleHeight)
}

plot_grid <- function(gt, bb, scale) {
	
	gridx <- pretty(bb[1,], n=gt$grid.n.x)
	gridx <- gridx[gridx>bb[1,1] & gridx<bb[1,2]]
	gridy <- pretty(bb[2,], n=gt$grid.n.y)
	gridy <- gridy[gridy>bb[2,1] & gridy<bb[2,2]]
	
	cogridx <- (gridx-bb[1,1]) / (bb[1,2] - bb[1,1])
	cogridy <- (gridy-bb[2,1]) / (bb[2,2] - bb[2,1])
	
	labelsx <- format(gridx, big.mark = ",")
	labelsy <- format(gridy, big.mark = ",")
	
	
	labelsYw <- max(convertWidth(stringWidth(labelsy), "npc", valueOnly=TRUE)) * .75
	labelsXw <- max(convertHeight(stringHeight(labelsx), "npc", valueOnly=TRUE)) * .75
	spacerY <- convertWidth(unit(.75, "lines"), unitTo="npc", valueOnly=TRUE)
	spacerX <- convertHeight(unit(.75, "lines"), unitTo="npc", valueOnly=TRUE)
	
	selx <- cogridx >= labelsYw + spacerY
	sely <- cogridy >= labelsXw + spacerX
	
	if (any(selx)) {
		cogridx <- cogridx[selx]
		labelsx <- labelsx[selx]
		
		grobGridX <- polylineGrob(x=rep(cogridx, each=2), y=rep(c(labelsXw+spacerX,1), length(cogridx)), 
					  id=rep(1:length(cogridx), each=2), gp=gpar(col=gt$grid.col, lwd=scale))
		grobGridTextX <- textGrob(labelsx, y=labelsXw+spacerX*.5, x=cogridx, just="top", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale))
	} else {
		grobGridX <- NULL
		grobGridTextX <- NULL
	}
	if (any(sely)) {
		cogridy <- cogridy[sely]
		labelsy <- labelsy[sely]
		
		grobGridY <- polylineGrob(y=rep(cogridy, each=2), x=rep(c(labelsYw+spacerY,1), length(cogridy)), 
					  id=rep(1:length(cogridy), each=2), gp=gpar(col=gt$grid.col, lwd=scale))
		grobGridTextY <- textGrob(labelsy, x=labelsYw+spacerY*.5, y=cogridy, just="right", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale))
	} else {
		grobGridY <- NULL
		grobGridTextY <- NULL
	}
	
	gTree(children=gList(grobGridX, grobGridY, grobGridTextX, grobGridTextY))
	
}

plot_bubbles <- function(co.npc, g, bubbleHeight) {
	with(g, {
		co.npc[, 1] <- co.npc[, 1] + bubble.xmod
		co.npc[, 2] <- co.npc[, 2] + bubble.ymod
		npol <- nrow(co.npc)
		if (length(bubble.size)!=npol) {
			if (length(bubble.size)!=1) warning("less bubble size values than objects")
			bubble.size <- rep(bubble.size, length.out=npol)
		}
		
		bubble.size <- bubble.size * bubbleHeight
		
		cols <- rep(bubble.col, length.out=npol)
		if (length(bubble.size)!=1) {
			decreasing <- order(-bubble.size)
			co.npc2 <- co.npc[decreasing,]
			bubble.size2 <- bubble.size[decreasing]
			cols2 <- if (length(cols)==1) cols else cols[decreasing]
		} else {
			co.npc2 <- co.npc
			bubble.size2 <- bubble.size
			cols2 <- cols
		}

		cols3 <- get_alpha_col(cols2, bubble.alpha)
		bordercol <- get_alpha_col(bubble.border.col, bubble.border.alpha)
		
		circleGrob(x=unit(co.npc2[,1], "npc"), y=unit(co.npc2[,2], "npc"),
					r=unit(bubble.size2, "inch"),
					gp=gpar(col=bordercol, lwd=bubble.border.lwd, fill=cols3))
	})
}

plot_text <- function(co.npc, g, just=c("center", "center"), bg.margin=.10) {
	npol <- nrow(co.npc)
	with(g, {
		if (!any(text_sel)) {
			warning("No text to display. Check if all size values are smaller than lowerbound.size, or if all positions fall outside the plotting area.")
			return(NULL)
		}
		
		co.npc[, 1] <- co.npc[, 1] + text.xmod
		co.npc[, 2] <- co.npc[, 2] + text.ymod
		
		grobText <- textGrob(text[text_sel], x=unit(co.npc[text_sel,1], "npc"), y=unit(co.npc[text_sel,2], "npc"), just=just, gp=gpar(col=text.fontcolor[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		nlines <- rep(1, length(text))
		
		
		lineH <- convertHeight(unit(text.size[text_sel], "lines"), "npc", valueOnly=TRUE)
		lineW <- convertWidth(unit(text.size[text_sel], "lines"), "npc", valueOnly=TRUE)

		if (!is.na(text.bg.color)) {
			
			
			tGH <- mapply(text[text_sel], text.size[text_sel], nlines[text_sel], FUN=function(x,y,z){
				convertHeight(grobHeight(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"npc", valueOnly=TRUE) * z/(z-0.25)}, USE.NAMES=FALSE)
			
			tGW <- mapply(text[text_sel], text.size[text_sel], FUN=function(x,y){
				convertWidth(grobWidth(textGrob(x, gp=gpar(cex=y, fontface=text.fontface, fontfamily=text.fontfamily))),"npc", valueOnly=TRUE)}, USE.NAMES=FALSE)
			tGX <- grobText$x + unit(ifelse(just[1]=="left", (tGW * .5), 
									  ifelse(just[1]=="right", -(tGW * .5), 0)), "npc")
			tGY <- grobText$y + unit(ifelse(just[2]=="top", -(tGH * .5), 
									  ifelse(just[2]=="bottom", tGH * .5, 0)), "npc")
		
			tGH <- tGH + lineH * bg.margin
			tGW <- tGW + lineW * bg.margin
			grobTextBG <- rectGrob(x=tGX, y=tGY, width=tGW, height=tGH, gp=gpar(fill=text.bg.color, col=NA))
		} else {
			grobTextBG <- NULL
		}
		
		if (text.shadow) {
			grobTextSh <- textGrob(text[text_sel], x=unit(co.npc[text_sel,1]+lineW * .05, "npc"), y=unit(co.npc[text_sel,2]- lineH * .05, "npc"), just=just, gp=gpar(col=text.shadowcol[text_sel], cex=text.size[text_sel], fontface=text.fontface, fontfamily=text.fontfamily))
		} else {
			grobTextSh <- NULL
		}
		
		gList(grobTextBG, grobTextSh, grobText)
	})
}


plot_all <- function(i, gp, shps.env, dasp, sasp, legend_pos) {
	gt <- gp$tm_layout
	
	gp[c("tm_layout")] <- NULL
	
	if (!gt$legend.only) {
		## determine aspvp
		margins <- gt$outer.margins
		height <- 1 - sum(margins[c(1,3)])
		width <- 1 - sum(margins[c(2,4)])
		if (dasp > sasp) {
			width <- width * (sasp/dasp)
		} else {
			height <- height * (dasp/sasp)
		}
		
		if (height==1) {
			heightP <- convertHeight(unit(1, "npc"), unitTo = "points", valueOnly = TRUE)
			height <- convertHeight(unit(heightP-1, "points"), unitTo="npc", valueOnly=TRUE)
		}

		if (width==1) {
			widthP <- convertWidth(unit(1, "npc"), unitTo = "points", valueOnly = TRUE)
			width <- convertWidth(unit(widthP-1, "points"), unitTo="npc", valueOnly=TRUE)
		}
		
		
		if (!gt$draw.frame) {
			grobBG <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA))
		} else {
			grobBG <- NULL
		}

		gridLayoutMap <- viewport(layout=grid.layout(3, 3, 
													 heights=unit(c(1, height, 1), 
													 			 c("null", "npc", "null")), 
													 widths=unit(c(1, width, 1), 
													 			c("null", "npc", "null"))),
								  name="maingrid")
		pushViewport(gridLayoutMap)
	
		treeMap <- cellplot2(2, 2, name="aspvp", e={
			if (gt$draw.frame) {
				grobBGframe <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA), name="mapBG")
			} else {
				grobBGframe <- NULL
			}
			res <- plot_map(i, gp, gt, shps.env)
			treeElemGrid <- res$treeElemGrid
			bubbleHeight <- res$bubbleHeight
			gList(grobBGframe, treeElemGrid)
		})
		
		## crop outside frame (also labels, bubbles)
		bgcol <- ifelse(gt$draw.frame, gt$outer.bg.color, gt$bg.color)
		
		treeBGtop <- cellplot2(1,1:3, e=rectGrob(gp=gpar(col=bgcol, fill=bgcol)), name="mapBGtop")
		treeBGleft <- cellplot2(2,1, e=rectGrob(gp=gpar(col=bgcol, fill=bgcol)), name="mapBGleft")
		treeBGright <- cellplot2(2,3, e=rectGrob(gp=gpar(col=bgcol, fill=bgcol)), name="mapBGright")
		treeBGbottom <- cellplot2(3,1:3, e=rectGrob(gp=gpar(col=bgcol, fill=bgcol)), name="mapBGbottom")
		treeFrame <- cellplot2(2,2, e={
			if (gt$draw.frame) rectGrob(gp=gpar(col="#000000", fill=NA, lwd=gt$frame.lwd)) else rectGrob(gp=gpar(col=gt$bg.color, fill=NA))
		}, name="mapFrame")
		
		treeMapX <- gTree(children=gList(grobBG, gTree(children=gList(treeMap, treeBGtop, treeBGleft, treeBGright, treeBGbottom, treeFrame), vp=gridLayoutMap, name="outer_map")), name="BG")
		
		upViewport()
	} else {
		bubbleHeight <- convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE) * gt$legend.text.size * 2
		treeMapX <- NULL
	}
	
	#find statistic variables
	leg <- legend_prepare(gp, gt, bubbleHeight)
	
	if (!is.null(leg)) {
		if (!gt$legend.only) {
			vpLeg <- vpPath("maingrid", "aspvp")
			d <- downViewport(vpLeg)
			grobLegendBG <- NULL
		} else {
			vpLeg <- current.viewport()
			grobLegendBG <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA))
		}
		treeLegend <- legend_plot(gt, leg, legend_pos)
		treeLegendX <- gTree(children=gList(grobLegendBG, treeLegend))
		
		
		if (!gt$legend.only) {
			treeMapX <- addGrob(treeMapX, child=treeLegendX, gPath=gPath("outer_map", "aspvp"))
			upViewport(d)
		} else {
			treeMapX <- treeLegendX
		}
		
	} else {
		treeLegendX <- NULL
	}
	treeMapX #gTree(children=gList(treeMapX, treeLegendX))
}