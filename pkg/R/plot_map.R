
plot_map <- function(i, gp, gt, shps, bb) {
	nlayers <- length(gp)
	
	
	## bubble height needed to align with bubbles in legend
	bubbleHeight <- convertHeight(unit(.5, "lines"), "inch", valueOnly=TRUE)
	
	
	## grid lines
	if (gt$grid.show) {
		treeGrid <- plot_grid(gt, scale=gt$scale, add.labels = gt$grid.labels.inside.frame)
	} else {
		treeGrid <- NULL
	}

	## credits
	if (gt$credits.show) {
		treeCred <- plot_cred(gt)
	} else {
		treeCred <- NULL
	}

	## scale bar
	if (gt$scale.show) {
		treeScale <- plot_scale(gt, bb)
	} else {
		treeScale <- NULL
	}
	
	## thematic map layers
	treeElements <- mapply(function(gpl, shp) {
		bb <- attr(shp, "bbox")
		
		## obtain coordinates (to draw bubbles and text)
		if (inherits(shp, "Spatial")) {
			if (inherits(shp, "SpatialLines")) {
				co <- gCentroid(shp, byid=TRUE)@coords
			} else {
				co <- coordinates(shp) # prefered over gCentroid since coordinates correspond to first (normally largest) polygon of each object
			}
			co.npc <- co
			co.npc[,1] <- (co.npc[,1]-bb[1,1]) / (bb[1, 2]-bb[1,1])
			co.npc[,2] <- (co.npc[,2]-bb[2,1]) / (bb[2, 2]-bb[2,1])
		} else {
			co.npc <- NA
		}
		
		
		plot_tm_fill <- function() {
			fill <- if (is.null(gpl$fill)) NA else gpl$fill
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
		
		plot_tm_raster <- function() {
			rast <- if (is.null(gpl$raster)) NA else gpl$raster
			bb_target <- attr(shp, "bbox")
			bb_real <- bbox(shp)
			
			if (all(abs(bb_real-bb_target)< 1e-3)) {
				width <- 1
				height <- 1
				cent <- rowMeans(bb_target)
			} else {
				width <- (bb_real[1,2] - bb_real[1,1]) / (bb_target[1,2] - bb_target[1,1])
				height <- (bb_real[2,2] - bb_real[2,1]) / (bb_target[2,2] - bb_target[2,1])
				cent <- rowMeans(bb_real)
			}
			
			x <- (cent[1] - bb_target[1,1]) / (bb_target[1,2] - bb_target[1,1])
			y <- (cent[2] - bb_target[2,1]) / (bb_target[2,2] - bb_target[2,1])
			
			#if (inherits(shp, "Spatial")) shp <- as(shp, "RasterLayer")
			rasterGrob(matrix(rast, ncol=shp@ncols, nrow=shp@nrows, byrow = TRUE), x=x, y=y, width=width, height=height)
		} 
		
		e <- environment()
		fnames <- paste("plot", gpl$plot.order, sep="_")
		grobs <- lapply(fnames, do.call, args=list(), envir=e)
		items <- do.call("gList", args =  grobs)
		gTree(children=items)
	}, gp, shps, SIMPLIFY=FALSE)
	
# 	if (gt$design.mode) {
# 		des
# 	}
	
	grobsElemGrid <- if (gt$grid.show && gt$grid.on.top) {
		do.call("gList", args = c(treeElements, list(treeGrid, treeCred, treeScale)))
	} else {
		do.call("gList", args = c(list(treeGrid), treeElements, list(treeCred, treeScale)))
	}
	list(treeElemGrid=gTree(children=grobsElemGrid, name="mapElements"), bubbleHeight=bubbleHeight)
}

plot_scale <- function(gt, bb) {
	xrange <- bb[1,2] - bb[1,1]
	xrange2 <- xrange/gt$unit.size
	
	if (is.null(gt$scale.breaks)) {
		ticks2 <- pretty(c(0, xrange2 / 5), 4)
	} else {
		ticks2 <- gt$scale.breaks
	}
	labels <- c(ticks2, gt$unit)
	
	n <- length(ticks2)
	ticks <- ticks2*gt$unit.size
	ticks3 <- ticks / xrange
	
	widths <- ticks3[2] - ticks3[1]
	x <- ticks3[1:(n-1)]
	
	size <- min(gt$scale.size, widths/max(convertWidth(stringWidth(paste(ticks2, " ")), "npc", TRUE)))

	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE) * size
	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	unitWidth <- convertWidth(stringWidth(paste(gt$unit, " ")), "npc", TRUE) * size
	width <- widths * n + unitWidth
	
	if (is.character(gt$scale.position)) {
		position <- 
			c(switch(gt$scale.position[1], 
					 left=mx+widths*.5,
					 center=(1-width)/2,
					 centre=(1-width)/2,
					 right=1-mx-width),
			  switch(gt$scale.position[2],
			  	   top=1-lineHeight*2,
			  	   center=.5,
			  	   centre=.5,
			  	   bottom=lineHeight*.5))	
	} else position <- gt$scale.position

	x <- x + position[1]
	xtext <- c(ticks3, ticks3[n] + widths*.5 + unitWidth*.5) + position[1]
	
	gTree(children=gList(
		rectGrob(x=x, y=position[2]+lineHeight, width = widths, height=lineHeight*.5, just=c("left", "bottom"), gp=gpar(col="black", fill=c("white", "black"))),
		textGrob(label=labels, x = xtext, y = position[2]+lineHeight*.5, just=c("center", "center"), gp=gpar(cex=size))))
	
	
}

plot_cred <- function(gt) {
	lineHeight <- convertHeight(unit(1, "lines"), "npc", valueOnly=TRUE)
	my <- lineHeight / 2
	mx <- convertWidth(convertHeight(unit(my, "npc"), "inch"), "npc", TRUE)
	
	# number of lines
	nlines <- length(strsplit(gt$credits.text, "\n")[[1]])
	
	size <- min((1-2*mx) / convertWidth(stringWidth(gt$credits.text), "npc", valueOnly=TRUE), gt$credits.size)
	
	width <- convertWidth(stringWidth(gt$credits.text), "npc", valueOnly=TRUE) * size
	height <- lineHeight * (nlines) * size
	
	if (is.character(gt$credits.position)) {
		position <- 
			c(switch(gt$credits.position[1], 
					 left=mx,
					 center=(1-width)/2,
					 centre=(1-width)/2,
					 right=1-mx-width),
			  switch(gt$credits.position[2],
			  	   top=1-height*.75,
			  	   center=.5,
			  	   centre=.5,
			  	   bottom=height*.75))	
	} else position <- gt$credits.position
	
	gTree(children=gList(if (!is.na(gt$credits.bg.color)) {
		bg.col <- get_alpha_col(gt$credits.bg.color, gt$credits.bg.alpha)
		rectGrob(x = position[1]-.5*mx, y = position[2], width=width+mx, just=c("left", "center"), height=height, gp=gpar(col=NA, fill=bg.col))
	} else {
		NULL
	}, textGrob(label=gt$credits.text, x = position[1], y = position[2], just=c("left", "center"), gp=gpar(cex=size))))
}

process_grid <- function(gt, bb) {
	within(gt, { 
		grid.x <- pretty(bb[1,], n=grid.n.x)
		grid.x <- grid.x[grid.x>bb[1,1] & grid.x<bb[1,2]]
		grid.y <- pretty(bb[2,], n=grid.n.y)
		grid.y <- grid.y[grid.y>bb[2,1] & grid.y<bb[2,2]]
		
		grid.co.x <- (grid.x-bb[1,1]) / (bb[1,2] - bb[1,1])
		grid.co.y <- (grid.y-bb[2,1]) / (bb[2,2] - bb[2,1])
		
		grid.labels.x <- format(grid.x, big.mark = ",")
		grid.labels.y <- format(grid.y, big.mark = ",")
	})
}

plot_grid_labels_x <- function(gt, scale) {
	cogridx <- gt$grid.co.x
	labelsx <- gt$grid.labels.x

	textGrob(labelsx, y=1, x=cogridx, just="top", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale))
		
}

plot_grid_labels_y <- function(gt, scale) {
	cogridy <- gt$grid.co.y
	labelsy <- gt$grid.labels.y
	
	textGrob(labelsy, y=cogridy, x=1, just="right", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale))
}


plot_grid <- function(gt, scale, add.labels) {
	cogridx <- gt$grid.co.x
	cogridy <- gt$grid.co.y
	labelsx <- gt$grid.labels.x
	labelsy <- gt$grid.labels.y
	
	
	if (add.labels) {
		labelsYw <- max(convertWidth(stringWidth(labelsy), "npc", valueOnly=TRUE)) * .75
		labelsXw <- max(convertHeight(stringHeight(labelsx), "npc", valueOnly=TRUE)) * .75
		spacerY <- convertWidth(unit(.75, "lines"), unitTo="npc", valueOnly=TRUE)
		spacerX <- convertHeight(unit(.75, "lines"), unitTo="npc", valueOnly=TRUE)
		selx <- cogridx >= labelsYw + spacerY
		sely <- cogridy >= labelsXw + spacerX
	} else {
		labelsXw <- labelsYw <- spacerX <- spacerY <- 0
		selx <- rep.int(TRUE, length(cogridx))
		sely <- rep.int(TRUE, length(cogridy))
	}	
	
	if (any(selx)) {
		cogridx <- cogridx[selx]
		labelsx <- labelsx[selx]
		
		grobGridX <- polylineGrob(x=rep(cogridx, each=2), y=rep(c(labelsXw+spacerX,1), length(cogridx)), 
					  id=rep(1:length(cogridx), each=2), gp=gpar(col=gt$grid.col, lwd=scale))
		grobGridTextX <- if (add.labels) {
			 textGrob(labelsx, y=labelsXw+spacerX*.5, x=cogridx, just="top", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale))
		} else NULL
	} else {
		grobGridX <- NULL
		grobGridTextX <- NULL
	}
	if (any(sely)) {
		cogridy <- cogridy[sely]
		labelsy <- labelsy[sely]
		
		grobGridY <- polylineGrob(y=rep(cogridy, each=2), x=rep(c(labelsYw+spacerY,1), length(cogridy)), 
					  id=rep(1:length(cogridy), each=2), gp=gpar(col=gt$grid.col, lwd=scale))
		grobGridTextY <- if (add.labels) {
			textGrob(labelsy, x=labelsYw+spacerY*.5, y=cogridy, just="right", gp=gpar(col=gt$grid.labels.col, cex=gt$grid.labels.size*scale))
		} else NULL
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

		bordercol <- get_alpha_col(bubble.border.col, bubble.border.alpha)
		
		circleGrob(x=unit(co.npc2[,1], "npc"), y=unit(co.npc2[,2], "npc"),
					r=unit(bubble.size2, "inch"),
					gp=gpar(col=bordercol, lwd=bubble.border.lwd, fill=cols2))
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



plot_all <- function(i, gp, shps.env, dasp, sasp, inner.margins.new, legend_pos) {
	gt <- gp$tm_layout

	shps <- get("shps", envir=shps.env)
	bb <- attr(shps[[1]], "bbox")
	
	## in case of small multiples, get i'th shape
	if (any(gt$shp_nr!=0) && gt$free.coords) {
		shps <- shps[[i]]
	}
	
	gt <- process_grid(gt, bb)
	
	
	
	
	gp[c("tm_layout")] <- NULL
	
	if (!gt$legend.only) {
		## calculate width and height of the shape based on the device asp ratio (dasp) and the shape aspect ratio (sasp)
		margins <- gt$outer.margins
		mar.y <- sum(margins[c(1,3)])
		mar.x <- sum(margins[c(2,4)])
		
		height <- 1 - mar.y
		width <- 1 - mar.x
		if (dasp > sasp) {
			width <- width * (sasp/dasp)
		} else {
			height <- height * (dasp/sasp)
		}
		
		## calculate outer margins
		margin.left <- (1 - width) * ifelse(mar.x==0, .5, (margins[2]/mar.x))
		margin.top <- (1 - height) * ifelse(mar.y==0, .5, (margins[3]/mar.y))
		

		## background rectangle (whole device)
		if (!gt$draw.frame) {
			grobBG <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA))
		} else {
			grobBG <- NULL
		}

		if (gt$design.mode) {
			grobBG <- rectGrob(gp=gpar(fill="yellow", col="yellow"))
		}
		
		## create a 3x3 grid layout with the shape to be drawn in the middle cell
		gridLayoutMap <- viewport(layout=grid.layout(3, 3, 
													 heights=unit(c(margin.top, height, 1), 
													 			 c("npc", "npc", "null")), 
													 widths=unit(c(margin.left, width, 1), 
													 			c("npc", "npc", "null"))),
								  name="maingrid")
		pushViewport(gridLayoutMap)

		## the thematic map with background
		treeMap <- cellplot(2, 2, name="aspvp", e={
			## background rectangle (inside frame)
			if (gt$draw.frame) {
				grobBGframe <- rectGrob(gp=gpar(fill=gt$bg.color, col=NA), name="mapBG")
			} else {
				grobBGframe <- NULL
			}
			
			if (gt$design.mode) {
				grobBGframe <- rectGrob(gp=gpar(fill="blue", col="blue"), name="mapBG")
				
				aspWidth <- 1-sum(inner.margins.new[c(2,4)])
				aspHeight <- 1-sum(inner.margins.new[c(1,3)])
				grobAsp <- rectGrob(x = (inner.margins.new[2]+1-inner.margins.new[4])/2, y=(inner.margins.new[1]+1-inner.margins.new[3])/2, width=aspWidth, height=aspHeight, gp=gpar(fill="red", col="red"), name="aspRect")
			} else {
				grobAsp <- NULL
			}
			
			## the thematic map
			res <- plot_map(i, gp, gt, shps, bb)
			treeElemGrid <- res$treeElemGrid
			bubbleHeight <- res$bubbleHeight
			gList(grobBGframe, grobAsp, treeElemGrid)
		})
		
		## background rectangle (whole device), in case a frame is drawn and outer.bg.color is specified
		treeBG <- if (!is.null(gt$outer.bg.color) && gt$draw.frame) {
			cellplot(1:3,1:3, e=rectGrob(gp=gpar(col=gt$outer.bg.color, fill=gt$outer.bg.color)), name="mapBG")
		} else NULL
		treeFrame <- cellplot(2,2, e={
			if (gt$draw.frame) rectGrob(gp=gpar(col="#000000", fill=NA, lwd=gt$frame.lwd)) else rectGrob(gp=gpar(col=gt$bg.color, fill=NA))
		}, name="mapFrame")
		
		treeGridLabels <- if (gt$grid.show && !gt$grid.labels.inside.frame) {
			gTree(children=gList(
				cellplot(3,2, e=plot_grid_labels_x(gt, scale=gt$scale), name="gridLabelsX"),
				cellplot(2,1, e=plot_grid_labels_y(gt, scale=gt$scale), name="gridLabelsY")), name="gridLabels")
		} else NULL
		
		
		
		treeMapX <- gTree(children=gList(grobBG, gTree(children=gList(treeBG, treeMap, treeFrame, treeGridLabels), vp=gridLayoutMap, name="outer_map")), name="BG")
		
		upViewport()
	} else {
		## bubble height needed to align with bubbles in legend
		bubbleHeight <- convertHeight(unit(1, "lines"), "inch", valueOnly=TRUE) * gt$legend.text.size * 2
		treeMapX <- NULL
	}
	
	## prepare legend items
	leg <- legend_prepare(gp, gt, bubbleHeight)
	
	## legend and title
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
	
	treeMapX
}